package io.getquill

import io.getquill.context.sql.idiom.SqlIdiom
import io.getquill.context.sql.idiom.QuestionMarkBindVariables
import io.getquill.context.sql.idiom.ConcatSupport

class MirrorSqlDialect[N <: NamingStrategy](val naming: N)
  extends SqlIdiom[N]
  with QuestionMarkBindVariables
  with ConcatSupport {

  override def prepareForProbing(string: String): String = string
}

