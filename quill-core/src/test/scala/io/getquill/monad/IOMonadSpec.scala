package io.getquill.monad

import io.getquill.Spec
import scala.util.Success
import scala.util.Try
import scala.util.Failure
import io.getquill.monad.Effect.Write

trait IOMonadSpec extends Spec {

  val ctx: IOMonad
  import ctx._

  def eval[T](io: IO[T, _]): T

  "IO companion object" - {
    "fromTry" - {
      "success" in {
        val t = Success(1)
        val io = IO.fromTry(t)
        Try(eval(io)) mustEqual t
      }
      "failure" in {
        val t = Failure(new Exception)
        val io = IO.fromTry(t)
        Try(eval(io)) mustEqual t
      }
    }
    "sequence" - {
      "empty" in {
        val io = IO.sequence(Seq.empty[IO[Int, Write]])
        eval(io) mustEqual Seq()
      }
      "non-empty" in {
        val io = IO.sequence(Seq(IO.successful(1), IO.successful(2)))
        eval(io) mustEqual Seq(1, 2)
      }
    }

    "transformWith" - {
      "base io" - {
        "success" in {
          val t = Success(1)
          val io = IO.fromTry(t).transformWith(IO.fromTry)
          Try(eval(io)) mustEqual t
        }
        "failure" in {
          val t = Failure[Int](new Exception)
          val io = IO.fromTry(t).transformWith(IO.fromTry)
          Try(eval(io)) mustEqual t
        }
      }
      "transformed io" - {
        "success" in {
          val t = Success(1)
          val io = IO.successful(()).transformWith(_ => IO.fromTry(t))
          Try(eval(io)) mustEqual t
        }
        "failure" in {
          val t = Failure[Int](new Exception)
          val io = IO.successful(()).transformWith(_ => IO.fromTry(t))
          Try(eval(io)) mustEqual t
        }
      }
      "transformation" - {
        "success" in {
          val io = IO.successful(1).transformWith(t => IO.fromTry(t.map(_ + 1)))
          Try(eval(io)) mustEqual Success(2)
        }
        "failure" in {
          val ex = new Exception
          val io = IO.successful(()).transformWith(_ => throw ex)
          Try(eval(io)) mustEqual Failure(ex)
        }
      }
    }

    "unit" in {
      eval(IO.unit) mustEqual (())
    }

    "transform" - {
      "base io" - {
        "success" in {
          val t = Success(1)
          val io = IO.fromTry(t).transform(identity)
          Try(eval(io)) mustEqual t
        }
        "failure" in {
          val t = Failure[Int](new Exception)
          val io = IO.fromTry(t).transform(identity)
          Try(eval(io)) mustEqual t
        }
      }
      "transformed try" - {
        "success" in {
          val t = Success(1)
          val io = IO.successful(()).transform(_ => t)
          Try(eval(io)) mustEqual t
        }
        "failure" in {
          val t = Failure[Int](new Exception)
          val io = IO.successful(()).transform(_ => t)
          Try(eval(io)) mustEqual t
        }
      }
      "transformation" - {
        "success" in {
          val io = IO.successful(1).transform(_.map(_ + 1))
          Try(eval(io)) mustEqual Success(2)
        }
        "failure" in {
          val ex = new Exception
          val io = IO.successful(()).transform(_ => throw ex)
          Try(eval(io)) mustEqual Failure(ex)
        }
      }
    }

    "zip" - {
      "success" in {
        val io = IO.zip(IO.successful(1), IO.successful(2))
        eval(io) mustEqual ((1, 2))
      }
      "failure" - {
        "left" in {
          val ex = new Exception
          val io = IO.zip(IO.failed(ex), IO.successful(2))
          Try(eval(io)) mustEqual Failure(ex)
        }
        "right" in {
          val ex = new Exception
          val io = IO.zip(IO.successful(1), IO.failed(ex))
          Try(eval(io)) mustEqual Failure(ex)
        }
      }
    }

    "failed" in {
      val ex = new Exception
      val io = IO.failed(ex)
      Try(eval(io)) mustEqual Failure(ex)
    }

    "successful" in {
      val io = IO.successful(1)
      eval(io) mustEqual 1
    }

    "apply" - {
      "success" in {
        val io = IO(1)
        eval(io) mustEqual 1
      }
      "failure" in {
        val ex = new Exception
        val io = IO(throw ex)
        Try(eval(io)) mustEqual Failure(ex)
      }
    }

    "foldLeft" - {
      "success" in {
        val ios = List(IO(1), IO(2))
        val io =
          IO.foldLeft(ios)(0) {
            case (a, b) => a + b
          }
        eval(io) mustEqual 3
      }
      "empty" in {
        val io =
          IO.foldLeft(List.empty[IO[Int, Effect]])(0) {
            case (a, b) => a + b
          }
        eval(io) mustEqual 0
      }
      "failure" - {
        "ios" in {
          val ex = new Exception
          val ios = List(IO(1), IO.failed[Int](ex))
          val io =
            IO.foldLeft(ios)(0) {
              case (a, b) => a + b
            }
          Try(eval(io)) mustEqual Failure(ex)
        }
        "op" in {
          val ex = new Exception
          val ios = List(IO(1), IO(2))
          val io =
            IO.foldLeft(ios)(0) {
              case (a, b) => throw ex
            }
          Try(eval(io)) mustEqual Failure(ex)
        }
      }
    }

    "reduceLeft" - {
      "success" in {
        val ios = List(IO(1), IO(2))
        val io =
          IO.reduceLeft(ios) {
            case (a, b) => a + b
          }
        eval(io) mustEqual 3
      }
      "empty" in {
        val io =
          IO.reduceLeft(List.empty[IO[Int, Effect]]) {
            case (a, b) => a + b
          }
        intercept[UnsupportedOperationException](eval(io))
      }
      "failure" - {
        "ios" in {
          val ex = new Exception
          val ios = List(IO(1), IO.failed[Int](ex))
          val io =
            IO.reduceLeft(ios) {
              case (a, b) => a + b
            }
          Try(eval(io)) mustEqual Failure(ex)
        }
        "op" in {
          val ex = new Exception
          val ios = List(IO(1), IO(2))
          val io =
            IO.reduceLeft(ios) {
              case (a, b) => throw ex
            }
          Try(eval(io)) mustEqual Failure(ex)
        }
      }
    }

    "traverse" - {
      "empty" in {
        val ios = List.empty[IO[Int, Effect]]
        val io = IO.traverse(ios)(IO.successful)
        eval(io) mustEqual List()
      }
      "success" in {
        val ints = List(1, 2)
        val io = IO.traverse(ints)(IO.successful)
        eval(io) mustEqual ints
      }
      "failure" in {
        val ex = new Exception
        val ints = List(1, 2)
        val io = IO.traverse(ints)(_ => throw ex)
        Try(eval(io)) mustEqual Failure(ex)
      }
    }
  }

  "IO instance" - {
    "zip" - {
      "success" in {
        val io = IO.successful(1).zip(IO.successful(2))
        eval(io) mustEqual ((1, 2))
      }
      "failure" - {
        "this" in {
          val ex = new Exception
          val io = IO.failed(ex).zip(IO.successful(2))
          Try(eval(io)) mustEqual Failure(ex)
        }
        "that" in {
          val ex = new Exception
          val io = IO.successful(1).zip(IO.failed(ex))
          Try(eval(io)) mustEqual Failure(ex)
        }
      }
    }
    "transform" - {
      "this" - {
        "success" in {
          val t = Success(1)
          val io = IO.fromTry(t).transform(identity)
          Try(eval(io)) mustEqual t
        }
        "failure" in {
          val t = Failure[Int](new Exception)
          val io = IO.fromTry(t).transform(identity)
          Try(eval(io)) mustEqual t
        }
      }
      "that" - {
        "success" in {
          val t = Success(1)
          val io = IO.successful(()).transform(_ => t)
          Try(eval(io)) mustEqual t
        }
        "failure" in {
          val t = Failure[Int](new Exception)
          val io = IO.successful(()).transform(_ => t)
          Try(eval(io)) mustEqual t
        }
      }
      "transformation" - {
        "success" in {
          val io = IO.successful(1).transform(_.map(_ + 1))
          Try(eval(io)) mustEqual Success(2)
        }
        "failure" in {
          val ex = new Exception
          val io = IO.successful(()).transform(_ => throw ex)
          Try(eval(io)) mustEqual Failure(ex)
        }
      }
    }
  }

}