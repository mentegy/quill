package io.getquill.context.sql.idiom

trait QuestionMarkBindVariables { self: SqlIdiom[_] =>

  override def liftingPlaceholder(index: Int): String = s"?"
}
