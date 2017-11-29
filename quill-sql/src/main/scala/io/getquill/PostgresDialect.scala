package io.getquill

import java.util.concurrent.atomic.AtomicInteger

import io.getquill.ast._
import io.getquill.context.sql.idiom.{ QuestionMarkBindVariables, SqlIdiom }
import io.getquill.idiom.StatementInterpolator._
import io.getquill.context.sql.idiom.ConcatSupport
import io.getquill.util.Messages.fail

trait PostgresDialect
  extends SqlIdiom
  with QuestionMarkBindVariables
  with ConcatSupport {

  override def astTokenizer(implicit astTokenizer: Tokenizer[Ast], strategy: NamingStrategy): Tokenizer[Ast] =
    Tokenizer[Ast] {
      case ListContains(ast, body) => stmt"${body.token} = ANY(${ast.token})"
      case ast                     => super.astTokenizer.token(ast)
    }

  override implicit def operationTokenizer(implicit astTokenizer: Tokenizer[Ast], strategy: NamingStrategy): Tokenizer[Operation] =
    Tokenizer[Operation] {
      case UnaryOperation(StringOperator.`toLong`, ast) => stmt"${scopedTokenizer(ast)}::bigint"
      case UnaryOperation(StringOperator.`toInt`, ast)  => stmt"${scopedTokenizer(ast)}::integer"
      case operation                                    => super.operationTokenizer.token(operation)
    }

  override implicit def actionTokenizer(implicit astTokenizer: Tokenizer[Ast], strategy: NamingStrategy): Tokenizer[Action] = {
    def tokenizer(implicit astTokenizer: Tokenizer[Ast]) =
      Tokenizer[Action] {
        case Conflict(a, NoTarget, DoNothingOnConflict) => stmt"${a.token} ON CONFLICT DO NOTHING"
        case Conflict(a, target, act)                   => stmt"${a.token} ON CONFLICT ${target.token} ${act.token}"
        case action                                     => super.actionTokenizer.token(action)
      }
    tokenizer(actionAstTokenizer)
  }

  implicit def conflictTargetTokenizer(implicit astTokenizer: Tokenizer[Ast], strategy: NamingStrategy): Tokenizer[ConflictTarget] =
    Tokenizer[ConflictTarget] {
      case NoTarget             => fail("Upsert without explicit conflict target is not supported")
      case ConstraintTarget(n)  => stmt"${n.token}"
      case ColumnsTarget(props) => stmt"(${props.map(p => strategy.column(p.name).token).mkStmt(",")})"
    }

  implicit def conflictActionTokenizer(implicit astTokenizer: Tokenizer[Ast], strategy: NamingStrategy): Tokenizer[ConflictAction] =
    Tokenizer[ConflictAction] {
      case DoNothingOnConflict =>
        stmt"DO NOTHING"
      case DoUpdateOnConflict(assignments, exclusions) =>
        val excls = exclusions.map(a => a.copy(value = Ident(s"EXCLUDED.${a.property.token}")))
        stmt"DO UPDATE SET ${(assignments ++ excls).token}"
    }

  private[getquill] val preparedStatementId = new AtomicInteger

  override def prepareForProbing(string: String) =
    s"PREPARE p${preparedStatementId.incrementAndGet.toString.token} AS $string"
}

object PostgresDialect extends PostgresDialect
