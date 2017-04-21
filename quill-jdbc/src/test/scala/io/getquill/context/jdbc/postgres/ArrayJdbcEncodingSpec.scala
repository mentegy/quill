package io.getquill.context.jdbc.postgres

import io.getquill.Spec

class ArrayJdbcEncodingSpec extends Spec {
  val ctx = testContext
  import ctx._

  case class StrWrap(str: String)
  implicit val strWrapEncode: MappedEncoding[StrWrap, String] = MappedEncoding(_.str)
  implicit val strWrapDecode: MappedEncoding[String, StrWrap] = MappedEncoding(StrWrap.apply)

  case class ArraysTestEntity(texts: Seq[StrWrap])
  val q = quote(querySchema[ArraysTestEntity]("ArraysTestEntity"))

  "Support all sql base types and builders to Seq" in {
    val expected = ArraysTestEntity(List("hey", "ho").map(StrWrap.apply))
    ctx.run(q.insert(lift(expected)))
    val actual = ctx.run(q).head
    println(actual)
    actual mustBe expected
  }

}
