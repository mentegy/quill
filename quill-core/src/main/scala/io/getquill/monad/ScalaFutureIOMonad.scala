package io.getquill.monad

import scala.util.Failure
import scala.util.Success
import io.getquill.context.Context
import scala.concurrent.Future
import scala.concurrent.ExecutionContext

trait ScalaFutureIOMonad extends IOMonad {
  this: Context[_, _] =>

  type Result[T] = Future[T]

  def unsafePerformIO[T](io: IO[T, _])(implicit ec: ExecutionContext): Result[T] =
    io match {
      case FromTry(v) => Future.fromTry(v)
      case Run(f)     => f()
      case Sequence(in, cbf) =>
        Future.sequence(in.map(unsafePerformIO))(cbf, ec)
      case TransformWith(a, fA) =>
        unsafePerformIO(a)
          .map(Success(_))
          .recover { case ex => Failure(ex) }
          .flatMap(v => unsafePerformIO(fA(v)))
    }
}
