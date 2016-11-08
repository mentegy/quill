package io.getquill.monad

import scala.language.higherKinds
import scala.util.Success
import scala.util.Try
import io.getquill.context.Context
import scala.annotation.tailrec

trait SyncIOMonad extends IOMonad {
  this: Context[_, _] =>

  type Result[T] = T

  def unsafePerformIO[T](io: IO[T, _]): Result[T] = {
    @tailrec def loop[U](io: IO[U, _]): Result[U] = {
      def flatten[Y, M[X] <: TraversableOnce[X]](seq: Sequence[Y, M, Effect]) =
        seq.in.foldLeft(IO.successful(seq.cbf())) {
          (builder, item) =>
            builder.flatMap(b => item.map(b += _))
        }.map(_.result())
      io match {
        case FromTry(v) => v.get
        case Run(f)     => f()
        case seq @ Sequence(_, _) =>
          loop(flatten(seq))
        case TransformWith(a, fA) =>
          a match {
            case FromTry(v) => loop(fA(v))
            case Run(r)     => loop(fA(Try(r())))
            case seq @ Sequence(_, _) =>
              loop(flatten(seq).transformWith(fA))
            case TransformWith(b, fB) =>
              loop(b.transformWith(fB(_).transformWith(fA)))
          }
      }
    }
    loop(io)
  }
}
