package io.getquill.context.sql.idiom

import io.getquill.util.Messages

trait ConcatSupport {
  this: SqlIdiom[_] =>

  override def concatFunction = "UNNEST"
}

trait NoConcatSupport {
  this: SqlIdiom[_] =>

  override def concatFunction = Messages.fail(s"`concatMap` not supported by ${this.getClass.getSimpleName}")
}