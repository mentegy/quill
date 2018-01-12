package io.getquill.util.sql

import io.getquill.{PostgresDialect, SnakeCase, SqlMirrorContext}
import io.getquill.context.sql.SqlContext

trait HasPagination {
  this: SqlContext[_, _] =>

}

object HasPagination {
  val ctx = new SqlMirrorContext(PostgresDialect, SnakeCase)
  import ctx._
  case class Person(id: Int, name: String)

  implicit class Pagination[T](q: Query[T]) {
    def page(request: PageRequest) = quote {

    }
  }

}


case class PageRequest(number: Int, size: Int, orderings: List[(String, String)])
case class Page[T](values: Seq[T])