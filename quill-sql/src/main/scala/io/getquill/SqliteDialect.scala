package io.getquill

import io.getquill.idiom.{ Token, StringToken }
import io.getquill.context.sql.idiom.SqlIdiom
import io.getquill.context.sql.idiom.QuestionMarkBindVariables
import io.getquill.context.sql.idiom.NoConcatSupport

class SqliteDialect[N <: NamingStrategy](val naming: N)
  extends SqlIdiom[N]
  with QuestionMarkBindVariables
  with NoConcatSupport {

  override def emptySetContainsToken(field: Token) = StringToken("0")

  override def prepareForProbing(string: String) = s"sqlite3_prepare_v2($string)"
}
