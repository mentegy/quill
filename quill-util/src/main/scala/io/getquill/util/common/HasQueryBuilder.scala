package io.getquill.util.common

import io.getquill.context.Context

trait HasQueryBuilder {
  this: Context[_, _] =>

  case class QueryBuilder[T](private val query: Quoted[Query[T]]) {

    def ifDefined[U: Encoder](value: Option[U])(f: Quoted[(Query[T], U) => Query[T]]): QueryBuilder[T] =
      value match {
        case None => this
        case Some(v) =>
          QueryBuilder {
            quote {
              f(query, lift(v))
            }
          }
      }

    def build: Quoted[Query[T]] = query
  }
}