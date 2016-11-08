package io.getquill.monad

import io.getquill.MirrorContext
import io.getquill.Spec

class SyncMonadIOSpec extends IOMonadSpec {

  override val ctx = new MirrorContext
  import ctx._

  override def eval[T](io: IO[T, _]) =
    unsafePerformIO[T](io)

}