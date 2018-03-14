package io.getquill

import java.util.concurrent.atomic.AtomicInteger

import io.getquill.ast._
import io.getquill.context.sql.idiom.{ QuestionMarkBindVariables, SqlIdiom }
import io.getquill.idiom.StatementInterpolator._
import io.getquill.context.sql.idiom.ConcatSupport

class PostgresDialect[N <: NamingStrategy](val naming: N)
  extends SqlIdiom[N]
  with QuestionMarkBindVariables
  with ConcatSupport {

  override def astTokenizer(implicit astTokenizer: Tokenizer[Ast]): Tokenizer[Ast] =
    Tokenizer[Ast] {
      case ListContains(ast, body) => stmt"${body.token} = ANY(${ast.token})"
      case ast                     => super.astTokenizer.token(ast)
    }

  override implicit def operationTokenizer(implicit astTokenizer: Tokenizer[Ast]): Tokenizer[Operation] =
    Tokenizer[Operation] {
      case UnaryOperation(StringOperator.`toLong`, ast) => stmt"${scopedTokenizer(ast)}::bigint"
      case UnaryOperation(StringOperator.`toInt`, ast)  => stmt"${scopedTokenizer(ast)}::integer"
      case operation                                    => super.operationTokenizer.token(operation)
    }

  override def prepareForProbing(string: String) = {
    var i = 0
    val query = string.flatMap(x => if (x != '?') s"$x" else {
      i += 1
      s"$$$i"
    })
    s"PREPARE p${PostgresDialect.preparedStatementId.incrementAndGet.toString.token} AS $query"
  }
}

object PostgresDialect {
  private[getquill] val preparedStatementId = new AtomicInteger
}