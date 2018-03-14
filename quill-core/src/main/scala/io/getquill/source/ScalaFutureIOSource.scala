package io.getquill.source

import io.getquill.context.ExpandQuery
import io.getquill.monad.IO
import io.getquill.monad.IO.{FromTry, Transactional, TransformWith}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

trait ScalaFutureIOSource {
  this: Source[_] =>

  type Result[T] = Future[T]

  protected def unsafePerformIo[T](io: IO[T], transactional: Boolean = true)(implicit ec: ExecutionContext): Future[T] =
    io match {
      case FromTry(v) => Future.fromTry(v)

      case Run(f)     => f()

      case TransformWith(a, fA) =>
        unsafePerformIo(a)
          .map(Success(_))
          .recover { case ex => Failure(ex) }
          .flatMap(v => unsafePerformIo(fA(v)))

      case Transactional(io) =>
        unsafePerformIo(io, transactional = true)
    }
}
