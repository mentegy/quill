package io.getquill.idiom

trait EncodingDsl {
  import EncodingDsl._


}


object EncodingDsl {
  trait SupportedType[T]

  private def supported[T] = new SupportedType[T] {}
}