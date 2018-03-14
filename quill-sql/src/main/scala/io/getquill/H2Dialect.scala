package io.getquill

import io.getquill.idiom.StatementInterpolator._
import java.util.concurrent.atomic.AtomicInteger
import io.getquill.context.sql.idiom.PositionalBindVariables
import io.getquill.context.sql.idiom.SqlIdiom
import io.getquill.context.sql.idiom.ConcatSupport

class H2Dialect[N <: NamingStrategy](val naming: N)
  extends SqlIdiom[N]
  with PositionalBindVariables
  with ConcatSupport {

  override def prepareForProbing(string: String) =
    s"PREPARE p${H2Dialect.preparedStatementId.incrementAndGet.toString.token} AS $string}"
}

object H2Dialect {
  private[getquill] val preparedStatementId = new AtomicInteger
}
