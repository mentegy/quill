package io.getquill.context.sql.dsl

import java.sql.Timestamp
import java.time.LocalDate
import java.util.Date

import io.getquill.Spec
import io.getquill.context.sql.SqlContext
import org.scalatest.BeforeAndAfterEach

import scala.collection.mutable.ListBuffer

trait ArrayEncodingSpec extends Spec with BeforeAndAfterEach {
  val ctx: SqlContext[_, _] with ArrayEncoding
  import ctx._

  // Support all sql base types and `Traversable` implementers
  case class ArraysTestEntity(
    texts:      List[String],
    decimals:   Seq[BigDecimal],
    bools:      Vector[Boolean],
    bytes:      ListBuffer[Byte],
    shorts:     Iterable[Short],
    ints:       IndexedSeq[Int],
    longs:      Set[Long],
    floats:     Seq[Float],
    doubles:    Seq[Double],
    dates1:     Seq[Date],
    timestamps: Seq[Timestamp],
    dates2:     Seq[LocalDate]
  )
  val q = quote(query[ArraysTestEntity])
  val e = ArraysTestEntity(List("test"), Seq(BigDecimal(2.33)), Vector(true, true), ListBuffer(1),
    Iterable(3), IndexedSeq(2), Set(1, 2, 3), Seq(1f, 2f), Seq(4d, 3d),
    Seq(new Timestamp(System.currentTimeMillis())), Seq(new Timestamp(System.currentTimeMillis())),
    Seq(LocalDate.now()))

  // Support Traversable encoding basing on MappedEncoding
  case class StrWrap(str: String)
  // it fails with NPE on the line above, moving this into Spec implementation fixed that, why?
  //implicit val strWrapEncode: MappedEncoding[StrWrap, String] = MappedEncoding(_.toString)
  //implicit val strWrapDecode: MappedEncoding[String, StrWrap] = MappedEncoding(StrWrap.apply)
  case class WrapEntity(texts: Seq[StrWrap])
  val wrapQ = quote(querySchema[WrapEntity]("ArraysTestEntity"))
  val wrapE = WrapEntity(List("hey", "ho").map(StrWrap.apply))
}
