package io.getquill.context.jdbc.postgres

import io.getquill.context.sql.dsl.ArrayEncodingSpec

class ArrayJdbcEncodingSpec extends ArrayEncodingSpec {
  val ctx = testContext
  import ctx._

  "Support all sql base types and `Traversable` implementers" in {
    ctx.run(q.insert(lift(e)))
    val x = ctx.run(q).head
    //val dec: BigDecimal = x.decimals.head
    //println(dec.toString())
    x mustEqual e
  }

  implicit val strWrapEncode: MappedEncoding[StrWrap, String] = MappedEncoding(_.str)
  implicit val strWrapDecode: MappedEncoding[String, StrWrap] = MappedEncoding(StrWrap.apply)
  "Support Traversable encoding basing on MappedEncoding" in {
    ctx.run(wrapQ.insert(lift(wrapE)))
    ctx.run(wrapQ).head mustBe wrapE
  }

  override protected def beforeEach(): Unit = {
    ctx.run(q.delete)
    ()
  }
}
