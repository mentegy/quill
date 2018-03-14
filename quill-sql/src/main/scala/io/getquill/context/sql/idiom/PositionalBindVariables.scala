package io.getquill.context.sql.idiom

trait PositionalBindVariables { self: SqlIdiom[_] =>

  override def liftingPlaceholder(index: Int): String = s"$$${index + 1}"
}
