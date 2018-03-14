package io.getquill.idiom

import io.getquill.NamingStrategy
import io.getquill.dsl.{ CoreDsl, LiftingDsl }
import io.getquill.context.QueryMacro
import io.getquill.context.ActionMacro
import io.getquill.monad.IO

import scala.language.experimental.macros

trait Idiom[N <: NamingStrategy] extends IdiomApi[N] with CoreDsl with LiftingDsl {

  type ActionRun

  def run[T](quoted: Quoted[T]): IO[T] = macro QueryMacro.runQuerySingle[T]

  def run[T](quoted: Quoted[Query[T]]): IO[List[T]] = macro QueryMacro.runIoQuery[T]

  def run(quoted: Quoted[Action[_]]): IO[ActionRun] = macro ActionMacro.runAction

  def run[T](quoted: Quoted[ActionReturning[_, T]]): IO[T] = macro ActionMacro.runActionReturning[T]

  def run(quoted: Quoted[BatchAction[Action[_]]]): IO[List[ActionRun]] = macro ActionMacro.runBatchAction

  def run[T](quoted: Quoted[BatchAction[ActionReturning[_, T]]]): IO[List[T]] = macro ActionMacro.runBatchActionReturning[T]
}