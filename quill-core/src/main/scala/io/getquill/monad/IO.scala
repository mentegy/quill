package io.getquill.monad

import io.getquill.monad.IO._

import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

trait IO[+T] {

  def transactional: IO[T] = Transactional(this)

  def transformWith[S](f: Try[T] => IO[S]): IO[S] =
    TransformWith(this, f)

  def transform[S](f: Try[T] => Try[S]): IO[S] =
    transformWith { r =>
      IO.fromTry(f(r))
    }

  def lowerFromTry[U](implicit ev: T => Try[U]): IO[U] =
    map(ev).flatMap {
      case Success(v) => IO.successful(v)
      case Failure(e) => IO.failed(e)
    }

  def liftToTry: IO[Try[T]] =
    transformWith(IO.successful)

  def failed: IO[Throwable] =
    transform {
      case Failure(t) => Success(t)
      case Success(v) => Failure(new NoSuchElementException("IO.failed not completed with a throwable."))
    }

  def map[S](f: T => S): IO[S] = transform(_.map(f))

  def flatMap[S](f: T => IO[S]): IO[S] =
    transformWith {
      case Success(s) => f(s)
      case Failure(_) => this.asInstanceOf[IO[S]]
    }

  def filter(p: T => Boolean): IO[T] =
    map { r => if (p(r)) r else throw new NoSuchElementException("IO.filter predicate is not satisfied") }

  final def withFilter(p: T => Boolean): IO[T] = filter(p)

  def collect[S](pf: PartialFunction[T, S]): IO[S] =
    map {
      r => pf.applyOrElse(r, (t: T) => throw new NoSuchElementException("IO.collect partial function is not defined at: " + t))
    }

  def recover[U >: T](pf: PartialFunction[Throwable, U]): IO[U] =
    transform {
      _ recover pf
    }

  def recoverWith[U >: T](pf: PartialFunction[Throwable, IO[U]]): IO[U] =
    transformWith {
      case Failure(t) => pf.applyOrElse(t, (_: Throwable) => this)
      case Success(_) => this
    }
}

object IO {

  def fromTry[T](result: Try[T]): IO[T] = FromTry(result)

  val unit: IO[Unit] = fromTry(Success(()))

  def failed[T](exception: Throwable): IO[T] = fromTry(Failure(exception))

  def successful[T](result: T): IO[T] = fromTry(Success(result))

  def apply[T](body: => T): IO[T] = fromTry(Try(body))

  case class FromTry[T](t: Try[T]) extends IO[T]
  case class TransformWith[T, S](io: IO[T], f: Try[T] => IO[S]) extends IO[S]
  case class Transactional[T](io: IO[T]) extends IO[T]

}


object TransformIO {
  def apply(io: IO[_])(f: PartialFunction[IO[_], IO[_]]): IO[_] = io match {
    case x if f.isDefinedAt(x) => f(x)
    case TransformWith(a, b) => TransformWith(apply(a)(f), b.andThen(r => apply(r)(f)))
    case Transactional(a) => Transactional(apply(a)(f))
    case _ => io
  }
}