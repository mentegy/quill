package io.getquill.quotation

import java.nio.ByteBuffer

import io.getquill.ast.OnConflict.{ Excluded, Existing }
import io.getquill.ast._
import io.getquill.quotation.Utils._
import io.getquill.quotation.RawEncoding._
import io.getquill.util.Messages.fail

import scala.annotation.{ switch, tailrec }
import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag

object AstDecode {
  def apply(ast: EncodedAst): Ast =
    apply(new BBDecoder(ByteBuffer.wrap(ast.bytes.getBytes), ast.dynamics.toArray))

  def apply(bb: BBDecoder): Ast = (bb.b.get(), bb) match {
    case EntityEncoding(ast)               => ast
    case FilterEncoding(ast)               => ast
    case MapEncoding(ast)                  => ast
    case FlatMapEncoding(ast)              => ast
    case ConcatMapEncoding(ast)            => ast
    case SortByEncoding(ast)               => ast
    case OrderingEncoding(ast)             => ast
    case GroupByEncoding(ast)              => ast
    case AggregationEncoding(ast)          => ast
    case TakeEncoding(ast)                 => ast
    case DropEncoding(ast)                 => ast
    case UnionEncoding(ast)                => ast
    case JoinEncoding(ast)                 => ast
    case FlatJoinEncoding(ast)             => ast
    case DistinctEncoding(ast)             => ast
    case NestedEncoding(ast)               => ast
    case InfixEncoding(ast)                => ast
    case FunctionEncoding(ast)             => ast
    case IdentEncoding(ast)                => ast
    case PropertyEncoding(ast)             => ast
    case OptionOperationEncoding(ast)      => ast
    case TraversableOperationEncoding(ast) => ast
    case IfEncoding(ast)                   => ast
    case AssignmentEncoding(ast)           => ast
    case OperationEncoding(ast)            => ast
    case ValueEncoding(ast)                => ast
    case BlockEncoding(ast)                => ast
    case ValEncoding(ast)                  => ast
    case ExcludedEncoding(ast)             => ast
    case ExistingEncoding(ast)             => ast
    case ActionEncoding(ast)               => ast
    case DynamicEncoding(ast)              => ast
    case QuotedReferenceEncoding(ast)      => ast
    case LiftEncoding(ast)                 => ast
  }
}

object AstEncode {
  def apply(ast: Ast): EncodedAst = {
    val bb = apply(new BBEncoder(), ast)
    EncodedAst(new String(bb.toArray), bb.dynamics)
  }

  def apply(bb: BBEncoder, ast: Ast): BBEncoder = ast match {
    case a: Entity               => EntityEncoding.encode(bb, a)
    case a: Filter               => FilterEncoding.encode(bb, a)
    case a: Map                  => MapEncoding.encode(bb, a)
    case a: FlatMap              => FlatMapEncoding.encode(bb, a)
    case a: ConcatMap            => ConcatMapEncoding.encode(bb, a)
    case a: SortBy               => SortByEncoding.encode(bb, a)
    case a: Ordering             => OrderingEncoding.encode(bb, a)
    case a: GroupBy              => GroupByEncoding.encode(bb, a)
    case a: Aggregation          => AggregationEncoding.encode(bb, a)
    case a: Take                 => TakeEncoding.encode(bb, a)
    case a: Drop                 => DropEncoding.encode(bb, a)
    case a: Union                => UnionEncoding.encode(bb, a)
    case a: Join                 => JoinEncoding.encode(bb, a)
    case a: FlatJoin             => FlatJoinEncoding.encode(bb, a)
    case a: Distinct             => DistinctEncoding.encode(bb, a)
    case a: Nested               => NestedEncoding.encode(bb, a)
    case a: Infix                => InfixEncoding.encode(bb, a)
    case a: Function             => FunctionEncoding.encode(bb, a)
    case a: Ident                => IdentEncoding.encode(bb, a)
    case a: Property             => PropertyEncoding.encode(bb, a)
    case a: OptionOperation      => OptionOperationEncoding.encode(bb, a)
    case a: TraversableOperation => TraversableOperationEncoding.encode(bb, a)
    case a: If                   => IfEncoding.encode(bb, a)
    case a: Assignment           => AssignmentEncoding.encode(bb, a)
    case a: Operation            => OperationEncoding.encode(bb, a)
    case a: Value                => ValueEncoding.encode(bb, a)
    case a: Block                => BlockEncoding.encode(bb, a)
    case a: Val                  => ValEncoding.encode(bb, a)
    case a: Excluded             => ExcludedEncoding.encode(bb, a)
    case a: Existing             => ExistingEncoding.encode(bb, a)
    case a: Action               => ActionEncoding.encode(bb, a)
    case a: Dynamic              => DynamicEncoding.encode(bb, a)
    case a: QuotedReference      => QuotedReferenceEncoding.encode(bb, a)
    case a: Lift                 => LiftEncoding.encode(bb, a)
    case a                       => fail(s"Cannot encode $a")
  }
}

abstract class RawEncoding[T](val magic: Byte) {
  def encode(bb: BBEncoder, x: T): BBEncoder
  def decode(bb: BBDecoder): T

  def unapply(arg: (Byte, BBDecoder)): Option[T] =
    if (arg._1 == magic) Some(decode(arg._2)) else None
}

object RawEncoding {

  import NonAstEncoding._

  object EntityEncoding extends RawEncoding[Entity](1) {

    def encode(bb: BBEncoder, x: Entity): BBEncoder = bb
      .put(magic)
      .putString(x.name)
      .putList[PropertyAlias](x.properties, p => bb.putList(p.path, bb.putString).putString(p.alias))

    def decode(bb: BBDecoder): Entity = {
      val name = bb.getString
      val props = bb.getList { b =>
        PropertyAlias(b.getList(_.getString), b.getString)
      }
      Entity(name, props)
    }
  }

  object FilterEncoding extends RawEncoding[Filter](2) {
    def encode(bb: BBEncoder, x: Filter): BBEncoder = bb.put(magic).putAsts(x.query, x.alias, x.body)
    def decode(bb: BBDecoder): Filter = {
      Filter(bb.getAst, bb.get[Ident], bb.getAst)
    }
  }

  object MapEncoding extends RawEncoding[Map](3) {
    def encode(bb: BBEncoder, x: Map): BBEncoder = bb.put(magic).putAsts(x.query, x.alias, x.body)
    def decode(bb: BBDecoder) = Map(bb.getAst, bb.get[Ident], bb.getAst)
  }

  object FlatMapEncoding extends RawEncoding[FlatMap](4) {
    def encode(bb: BBEncoder, x: FlatMap): BBEncoder = bb.put(magic).putAsts(x.query, x.alias, x.body)
    def decode(bb: BBDecoder) = FlatMap(bb.getAst, bb.get[Ident], bb.getAst)
  }

  object ConcatMapEncoding extends RawEncoding[ConcatMap](5) {
    def encode(bb: BBEncoder, x: ConcatMap): BBEncoder = bb.put(magic).putAsts(x.query, x.alias, x.body)
    def decode(bb: BBDecoder) = ConcatMap(bb.getAst, bb.get[Ident], bb.getAst)
  }

  object SortByEncoding extends RawEncoding[SortBy](6) {
    def encode(bb: BBEncoder, x: SortBy): BBEncoder = bb.put(magic).putAsts(x.query, x.alias, x.criterias, x.ordering)
    def decode(bb: BBDecoder) = SortBy(bb.getAst, bb.get[Ident], bb.getAst, bb.getAst)
  }

  object OrderingEncoding extends RawEncoding[Ordering](7) {

    def encode(bb: BBEncoder, x: Ordering): BBEncoder = {
      bb.put(magic)
      x match {
        case TupleOrdering(ls) => bb.put(0).putList[Ordering](ls, OrderingEncoding.encode(bb, _))
        case Asc               => bb.put(1)
        case Desc              => bb.put(2)
        case AscNullsFirst     => bb.put(3)
        case DescNullsFirst    => bb.put(4)
        case AscNullsLast      => bb.put(5)
        case DescNullsLast     => bb.put(6)
      }
    }

    def decode(bb: BBDecoder): Ordering = (bb.b.get(): @switch) match {
      case 0 => TupleOrdering(bb.getList(OrderingEncoding.decode))
      case 1 => Asc
      case 2 => Desc
      case 3 => AscNullsFirst
      case 4 => DescNullsFirst
      case 5 => AscNullsLast
      case 6 => DescNullsLast
    }
  }

  object GroupByEncoding extends RawEncoding[GroupBy](8) {
    def encode(bb: BBEncoder, x: GroupBy): BBEncoder = bb.put(magic).putAsts(x.query, x.alias, x.body)
    def decode(bb: BBDecoder) = GroupBy(bb.getAst, bb.get[Ident], bb.getAst)
  }

  object AggregationEncoding extends RawEncoding[Aggregation](9) {
    def encode(bb: BBEncoder, x: Aggregation): BBEncoder = bb.put(magic).putAggregationOperator(x.operator).putAst(x.ast)
    def decode(bb: BBDecoder) = Aggregation(bb.getAggregationOperator, bb.getAst)
  }

  object TakeEncoding extends RawEncoding[Take](10) {
    def encode(bb: BBEncoder, x: Take): BBEncoder = bb.put(magic).putAsts(x.query, x.n)
    def decode(bb: BBDecoder) = Take(bb.getAst, bb.getAst)
  }

  object DropEncoding extends RawEncoding[Drop](11) {
    def encode(bb: BBEncoder, x: Drop): BBEncoder = bb.put(magic).putAsts(x.query, x.n)
    def decode(bb: BBDecoder) = Drop(bb.getAst, bb.getAst)
  }

  object UnionEncoding extends RawEncoding[Union](12) {
    def encode(bb: BBEncoder, x: Union): BBEncoder = bb.put(magic).putAsts(x.a, x.b)
    def decode(bb: BBDecoder) = Union(bb.getAst, bb.getAst)
  }

  object UnionAllEncoding extends RawEncoding[UnionAll](13) {
    def encode(bb: BBEncoder, x: UnionAll): BBEncoder = bb.put(magic).putAsts(x.a, x.b)
    def decode(bb: BBDecoder) = UnionAll(bb.getAst, bb.getAst)
  }

  object JoinEncoding extends RawEncoding[Join](14) {
    def encode(bb: BBEncoder, x: Join): BBEncoder =
      bb.put(magic).putJoinType(x.typ).putAsts(x.a, x.b, x.aliasA, x.aliasB, x.on)
    def decode(bb: BBDecoder) = Join(bb.getJoinType, bb.getAst, bb.getAst, bb.get[Ident], bb.get[Ident], bb.getAst)
  }

  object FlatJoinEncoding extends RawEncoding[FlatJoin](15) {
    def encode(bb: BBEncoder, x: FlatJoin): BBEncoder =
      bb.put(magic).putJoinType(x.typ).putAsts(x.a, x.aliasA, x.on)
    def decode(bb: BBDecoder) = FlatJoin(bb.getJoinType, bb.getAst, bb.get[Ident], bb.getAst)
  }

  object DistinctEncoding extends RawEncoding[Distinct](16) {
    def encode(bb: BBEncoder, x: Distinct): BBEncoder = bb.put(magic).putAsts(x.a)
    def decode(bb: BBDecoder) = Distinct(bb.getAst)
  }

  object NestedEncoding extends RawEncoding[Nested](17) {
    def encode(bb: BBEncoder, x: Nested): BBEncoder = bb.put(magic).putAsts(x.a)
    def decode(bb: BBDecoder) = Nested(bb.getAst)
  }

  object InfixEncoding extends RawEncoding[Infix](18) {
    def encode(bb: BBEncoder, x: Infix): BBEncoder =
      bb.put(magic).putList(x.parts, bb.putString).putList(x.params, bb.putAst)
    def decode(bb: BBDecoder) = Infix(bb.getList[String](_.getString), bb.getList[Ast](_.getAst))
  }

  object FunctionEncoding extends RawEncoding[Function](19) {
    def encode(bb: BBEncoder, x: Function): BBEncoder =
      bb.put(magic).putList(x.params, bb.putAst).putAst(x.body)
    def decode(bb: BBDecoder) = Function(bb.getList[Ident](_.get[Ident]), bb.getAst)
  }

  object IdentEncoding extends RawEncoding[Ident](20) {
    def encode(bb: BBEncoder, x: Ident): BBEncoder = bb.put(magic).putString(x.name)
    def decode(bb: BBDecoder) = Ident(bb.getString)
  }

  object PropertyEncoding extends RawEncoding[Property](21) {
    def encode(bb: BBEncoder, x: Property): BBEncoder = bb.put(magic).putAst(x.ast).putString(x.name)
    def decode(bb: BBDecoder) = Property(bb.getAst, bb.getString)
  }

  object OptionOperationEncoding extends RawEncoding[OptionOperation](22) {
    def encode(bb: BBEncoder, x: OptionOperation): BBEncoder = {
      bb.put(magic)
      x match {
        case OptionFlatten(a)       => bb.put(1).putAsts(a)
        case OptionGetOrElse(a, b)  => bb.put(2).putAsts(a, b)
        case OptionFlatMap(a, b, c) => bb.put(3).putAsts(a, b, c)
        case OptionMap(a, b, c)     => bb.put(4).putAsts(a, b, c)
        case OptionForall(a, b, c)  => bb.put(5).putAsts(a, b, c)
        case OptionExists(a, b, c)  => bb.put(6).putAsts(a, b, c)
        case OptionContains(a, b)   => bb.put(7).putAsts(a, b)
        case OptionIsEmpty(a)       => bb.put(8).putAsts(a)
        case OptionNonEmpty(a)      => bb.put(9).putAsts(a)
        case OptionIsDefined(a)     => bb.put(10).putAsts(a)
      }
    }
    def decode(bb: BBDecoder): OptionOperation = (bb.b.get(): @switch) match {
      case 1     => OptionFlatten(bb.getAst)
      case 2     => OptionGetOrElse(bb.getAst, bb.getAst)
      case 3     => OptionFlatMap(bb.getAst, bb.get[Ident], bb.getAst)
      case 4     => OptionMap(bb.getAst, bb.get[Ident], bb.getAst)
      case 5     => OptionForall(bb.getAst, bb.get[Ident], bb.getAst)
      case 6     => OptionExists(bb.getAst, bb.get[Ident], bb.getAst)
      case 7     => OptionContains(bb.getAst, bb.getAst)
      case 8     => OptionIsEmpty(bb.getAst)
      case 9     => OptionNonEmpty(bb.getAst)
      case 10    => OptionIsDefined(bb.getAst)
      case other => fail(s"Unknown OptionOperation $other")
    }
  }

  object TraversableOperationEncoding extends RawEncoding[TraversableOperation](23) {
    def encode(bb: BBEncoder, x: TraversableOperation): BBEncoder = {
      bb.put(magic)
      x match {
        case MapContains(a, b)  => bb.put(1).putAsts(a, b)
        case SetContains(a, b)  => bb.put(2).putAsts(a, b)
        case ListContains(a, b) => bb.put(3).putAsts(a, b)
      }
    }
    def decode(bb: BBDecoder): TraversableOperation = (bb.b.get(): @switch) match {
      case 1     => MapContains(bb.getAst, bb.getAst)
      case 2     => SetContains(bb.getAst, bb.getAst)
      case 3     => ListContains(bb.getAst, bb.getAst)
      case other => fail(s"Unknown TraversableOperation $other")
    }
  }

  object IfEncoding extends RawEncoding[If](24) {
    def encode(bb: BBEncoder, x: If): BBEncoder = bb.put(magic).putAsts(x.condition, x.`then`, x.`else`)
    def decode(bb: BBDecoder) = If(bb.getAst, bb.getAst, bb.getAst)
  }

  object AssignmentEncoding extends RawEncoding[Assignment](25) {
    def encode(bb: BBEncoder, x: Assignment): BBEncoder = bb.put(magic).putAsts(x.alias, x.property, x.value)
    def decode(bb: BBDecoder) = Assignment(bb.get[Ident], bb.getAst, bb.getAst)
  }

  object OperationEncoding extends RawEncoding[Operation](26) {
    def encode(bb: BBEncoder, x: Operation): BBEncoder = {
      bb.put(magic)
      x match {
        case UnaryOperation(a, b)     => bb.put(1).putUnaryOperator(a).putAst(b)
        case BinaryOperation(a, b, c) => bb.put(2).putAst(a).putBinaryOperator(b).putAst(c)
        case FunctionApply(a, b)      => bb.put(3).putAst(a).putList(b, bb.putAst)
      }
    }
    def decode(bb: BBDecoder): Operation = (bb.b.get(): @switch) match {
      case 1     => UnaryOperation(bb.getUnaryOperator, bb.getAst)
      case 2     => BinaryOperation(bb.getAst, bb.getBinaryOperator, bb.getAst)
      case 3     => FunctionApply(bb.getAst, bb.getList(_.getAst))
      case other => fail(s"Unknown Operation $other")
    }
  }

  object ValueEncoding extends RawEncoding[Value](27) {
    def encode(bb: BBEncoder, x: Value): BBEncoder = {
      bb.put(magic)
      x match {
        case Constant(a)  => bb.put(1).putTree(a)
        case NullValue    => bb.put(2)
        case Tuple(a)     => bb.put(3).putList(a, bb.putAst)
        case CaseClass(a) => bb.put(4).putList[(String, Ast)](a, { case (a, b) => bb.putString(a).putAst(b) })
      }
    }
    def decode(bb: BBDecoder): Value = (bb.b.get(): @switch) match {
      case 1     => Constant(bb.getTree)
      case 2     => NullValue
      case 3     => Tuple(bb.getList(_.getAst))
      case 4     => CaseClass(bb.getList(bb => (bb.getString, bb.getAst)))
      case other => fail(s"Unknown Operation $other")
    }
  }

  object BlockEncoding extends RawEncoding[Block](28) {
    def encode(bb: BBEncoder, x: Block): BBEncoder = bb.put(magic).putList(x.statements, bb.putAst)
    def decode(bb: BBDecoder) = Block(bb.getList(_.getAst))
  }

  object ValEncoding extends RawEncoding[Val](29) {
    def encode(bb: BBEncoder, x: Val): BBEncoder = bb.put(magic).putAsts(x.name, x.body)
    def decode(bb: BBDecoder) = Val(bb.get[Ident], bb.getAst)
  }

  object ExcludedEncoding extends RawEncoding[Excluded](30) {
    def encode(bb: BBEncoder, x: Excluded): BBEncoder = bb.put(magic).putAst(x.alias)
    def decode(bb: BBDecoder) = Excluded(bb.get[Ident])
  }

  object ExistingEncoding extends RawEncoding[Existing](31) {
    def encode(bb: BBEncoder, x: Existing): BBEncoder = bb.put(magic).putAst(x.alias)
    def decode(bb: BBDecoder) = Existing(bb.get[Ident])
  }

  object ActionEncoding extends RawEncoding[Action](32) {
    def encode(bb: BBEncoder, x: Action): BBEncoder = {
      bb.put(magic)
      x match {
        case Update(a, b)        => bb.put(1).putAst(a).putList(b, bb.putAst)
        case Insert(a, b)        => bb.put(2).putAst(a).putList(b, bb.putAst)
        case Delete(a)           => bb.put(3).putAst(a)
        case Returning(a, b, c)  => bb.put(4).putAsts(a, b, c)
        case Foreach(a, b, c)    => bb.put(4).putAsts(a, b, c)
        case OnConflict(a, b, c) => bb.put(4).putAst(a).putOnConflictTarget(b).putOnConflictAction(c)
      }
    }
    def decode(bb: BBDecoder): Action = (bb.b.get(): @switch) match {
      case 1     => Update(bb.getAst, bb.getList(_.get[Assignment]))
      case 2     => Insert(bb.getAst, bb.getList(_.get[Assignment]))
      case 3     => Delete(bb.getAst)
      case 4     => Returning(bb.getAst, bb.get[Ident], bb.getAst)
      case 5     => Foreach(bb.getAst, bb.get[Ident], bb.getAst)
      case 6     => OnConflict(bb.getAst, bb.getOnConflictTarget, bb.getOnConflictAction)
      case other => fail(s"Unknown Action $other")
    }
  }

  object DynamicEncoding extends RawEncoding[Dynamic](33) {
    def encode(bb: BBEncoder, x: Dynamic): BBEncoder = bb.put(magic).putTree(x.tree)
    def decode(bb: BBDecoder) = Dynamic(bb.getTree)
  }

  object QuotedReferenceEncoding extends RawEncoding[QuotedReference](34) {
    def encode(bb: BBEncoder, x: QuotedReference): BBEncoder = bb.put(magic).putTree(x.tree).putAst(x.ast)
    def decode(bb: BBDecoder) = QuotedReference(bb.getTree, bb.getAst)
  }

  object LiftEncoding extends RawEncoding[Lift](35) {
    def encode(bb: BBEncoder, x: Lift): BBEncoder = {
      bb.put(magic)
      x match {
        case ScalarValueLift(a, b, c) => bb.put(1).putString(a).putTree(b).putTree(c)
        case ScalarQueryLift(a, b, c) => bb.put(1).putString(a).putTree(b).putTree(c)
        case CaseClassValueLift(a, b) => bb.put(1).putString(a).putTree(b)
        case CaseClassQueryLift(a, b) => bb.put(1).putString(a).putTree(b)
      }
    }
    def decode(bb: BBDecoder): Lift = (bb.b.get(): @switch) match {
      case 1     => ScalarValueLift(bb.getString, bb.getTree, bb.getTree)
      case 2     => ScalarQueryLift(bb.getString, bb.getTree, bb.getTree)
      case 3     => CaseClassValueLift(bb.getString, bb.getTree)
      case 4     => CaseClassQueryLift(bb.getString, bb.getTree)
      case other => fail(s"Unknown Lift $other")
    }
  }
}

object NonAstEncoding {

  implicit class BBEncoderOps(bb: BBEncoder) {
    def putUnaryOperator(op: UnaryOperator): BBEncoder =
      bb.put(op match {
        case BooleanOperator.`!`          => 1
        case NumericOperator.`-`          => 2
        case StringOperator.`toUpperCase` => 3
        case StringOperator.`toLowerCase` => 4
        case StringOperator.`toLong`      => 5
        case StringOperator.`toInt`       => 6
        case SetOperator.`nonEmpty`       => 7
        case SetOperator.`isEmpty`        => 8
      })

    def putBinaryOperator(op: BinaryOperator): BBEncoder =
      bb.put(op match {
        case EqualityOperator.`==`       => 1
        case EqualityOperator.`!=`       => 2
        case BooleanOperator.`&&`        => 3
        case BooleanOperator.`||`        => 4
        case StringOperator.`+`          => 5
        case StringOperator.`startsWith` => 6
        case StringOperator.`split`      => 7
        case NumericOperator.`-`         => 8
        case NumericOperator.`+`         => 9
        case NumericOperator.`*`         => 10
        case NumericOperator.`>`         => 11
        case NumericOperator.`>=`        => 12
        case NumericOperator.`<`         => 13
        case NumericOperator.`<=`        => 14
        case NumericOperator.`/`         => 15
        case NumericOperator.`%`         => 16
        case SetOperator.`contains`      => 17
      })

    def putAggregationOperator(op: AggregationOperator): BBEncoder =
      bb.put(op match {
        case AggregationOperator.`min`  => 1
        case AggregationOperator.`max`  => 2
        case AggregationOperator.`avg`  => 3
        case AggregationOperator.`sum`  => 4
        case AggregationOperator.`size` => 5
      })

    def putJoinType(jt: JoinType): BBEncoder =
      bb.put(jt match {
        case InnerJoin => 1
        case LeftJoin  => 2
        case RightJoin => 3
        case FullJoin  => 4
      })

    def putOnConflictTarget(t: OnConflict.Target): BBEncoder =
      t match {
        case OnConflict.NoTarget      => bb.put(1)
        case OnConflict.Properties(p) => bb.put(2).putList(p, bb.putAst)
      }

    def putOnConflictAction(t: OnConflict.Action): BBEncoder =
      t match {
        case OnConflict.Ignore    => bb.put(1)
        case OnConflict.Update(p) => bb.put(2).putList(p, bb.putAst)
      }
  }

  implicit class BBDecoderOps(bb: BBDecoder) {
    def getUnaryOperator: UnaryOperator = (bb.b.get(): @switch) match {
      case 1     => BooleanOperator.`!`
      case 2     => NumericOperator.`-`
      case 3     => StringOperator.`toUpperCase`
      case 4     => StringOperator.`toLowerCase`
      case 5     => StringOperator.`toLong`
      case 6     => StringOperator.`toInt`
      case 7     => SetOperator.`nonEmpty`
      case 8     => SetOperator.`isEmpty`
      case other => fail(s"Unknown UnaryOperator $other")
    }

    def getBinaryOperator: BinaryOperator = (bb.b.get(): @switch) match {
      case 1     => EqualityOperator.`==`
      case 2     => EqualityOperator.`!=`
      case 3     => BooleanOperator.`&&`
      case 4     => BooleanOperator.`||`
      case 5     => StringOperator.`+`
      case 6     => StringOperator.`startsWith`
      case 7     => StringOperator.`split`
      case 8     => NumericOperator.`-`
      case 9     => NumericOperator.`+`
      case 10    => NumericOperator.`*`
      case 11    => NumericOperator.`>`
      case 12    => NumericOperator.`>=`
      case 13    => NumericOperator.`<`
      case 14    => NumericOperator.`<=`
      case 15    => NumericOperator.`/`
      case 16    => NumericOperator.`%`
      case 17    => SetOperator.`contains`
      case other => fail(s"Unknown BinaryOperator $other")
    }

    def getAggregationOperator: AggregationOperator = (bb.b.get(): @switch) match {
      case 1     => AggregationOperator.`min`
      case 2     => AggregationOperator.`max`
      case 3     => AggregationOperator.`avg`
      case 4     => AggregationOperator.`sum`
      case 5     => AggregationOperator.`size`
      case other => fail(s"Unknown AggregationOperator $other")
    }

    def getJoinType: JoinType = (bb.b.get(): @switch) match {
      case 1     => InnerJoin
      case 2     => LeftJoin
      case 3     => RightJoin
      case 4     => FullJoin
      case other => fail(s"Unknown JoinType $other")
    }

    def getOnConflictTarget: OnConflict.Target = (bb.b.get(): @switch) match {
      case 1     => OnConflict.NoTarget
      case 2     => OnConflict.Properties(bb.getList(_.get[Property]))
      case other => fail(s"Unknown OnConflict.Target $other")
    }

    def getOnConflictAction: OnConflict.Action = (bb.b.get(): @switch) match {
      case 1     => OnConflict.Ignore
      case 2     => OnConflict.Update(bb.getList(_.get[Assignment]))
      case other => fail(s"Unknown OnConflict.Action $other")
    }
  }
}

private[quotation] object Utils {
  val DefaultBBSize = 1024
  def alloc(bytes: Int): ByteBuffer = ByteBuffer.allocate(math.max(DefaultBBSize, bytes))

  class BBEncoder {
    private var body = List(alloc(0))
    private val dyn = ListBuffer.empty[Any]

    def dynamics: List[Any] = dyn.toList

    private def put[T, R](n: Int, f: ByteBuffer => R): BBEncoder = {
      if (body.head.remaining() > n) f(body.head)
      else {
        val bb = alloc(n)
        body ::= bb
        f(bb)
      }
      this
    }

    def putTree(any: Any): BBEncoder = {
      dyn.append(any)
      putInt(dyn.size - 1)
    }

    def putAst(ast: Ast): BBEncoder = AstEncode(this, ast)

    def putAsts(asts: Ast*): BBEncoder = asts.foldLeft(this)(AstEncode(_, _))

    def putString(x: String): BBEncoder = {
      val b = x.getBytes("utf-8")
      putInt(b.length)
      put(b.length, _.put(b))
    }

    def putList[T](list: List[T], f: T => BBEncoder): BBEncoder = {
      putInt(0)
      val bb = body.head
      val pos = bb.position() - 4
      var length = 0
      list.foreach { x =>
        length += 1
        f(x)
      }
      bb.putInt(pos, length)
      this
    }

    def put(x: Byte): BBEncoder = put(1, _.put(x))
    def putShort(x: Short): BBEncoder = put(2, _.putShort(x))
    def putInt(x: Int): BBEncoder = put(4, _.putInt(x))

    def toArray: Array[Byte] = {
      val arr = new Array[Byte](body.map(_.flip().remaining()).sum)

      @tailrec
      def iter(ls: List[ByteBuffer], i: Int): Array[Byte] = ls match {
        case Nil => arr
        case bb :: tail =>
          val size = bb.position()
          val pos = i - size
          Array.copy(bb.array(), 0, arr, pos, size)
          iter(tail, pos)
      }

      iter(body, arr.length)
    }
  }

  class BBDecoder(val b: ByteBuffer, dynamics: Array[Any]) {
    def getString: String = {
      val length = b.getInt
      val arr = new Array[Byte](length)
      b.get(arr)
      new String(arr, "utf-8")
    }

    def getList[T](f: BBDecoder => T): List[T] = {
      val ls = ListBuffer.empty[T]
      (1 to b.getInt).foreach { _ =>
        ls.append(f(this))
      }
      ls.toList
    }

    def getAst: Ast = get[Ast]

    def get[T <: Ast](implicit ct: ClassTag[T]): T = AstDecode(this) match {
      case ast: T => ast
      case other => fail(s"Expected ast of type ${ct.runtimeClass.getCanonicalName}, " +
        s"but found ${other.getClass.getCanonicalName}")
    }

    def getTree: Any = {
      val i = b.getInt()
      if (i >= dynamics.length) {
        fail(s"No dynamic part at $i")
      }
      dynamics(i)
    }
  }
}