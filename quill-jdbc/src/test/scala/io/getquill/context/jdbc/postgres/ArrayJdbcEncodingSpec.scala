package io.getquill.context.jdbc.postgres

import io.getquill.Spec
import org.scalatest.BeforeAndAfterEach

class ArrayJdbcEncodingSpec extends Spec with BeforeAndAfterEach {
  val ctx = testContext
  import ctx._

  case class ArraysTestEntity(texts: List[String])
  val q = quote(query[ArraysTestEntity])

  "Support all sql base types and `Traversable` implementers" in {
    val expected = ArraysTestEntity(List("hey", "ho"))
    ctx.run(q.insert(lift(expected)))
    ctx.run(q).head mustBe expected
  }

  case class StrWrap(str: String)
  implicit val strWrapEncode: MappedEncoding[StrWrap, String] = MappedEncoding(_.str)
  implicit val strWrapDecode: MappedEncoding[String, StrWrap] = MappedEncoding(StrWrap.apply)
  case class WrapEntity(texts: Seq[StrWrap])
  val wrapQ = quote(querySchema[WrapEntity]("ArraysTestEntity"))

  "Support Traversable encoding basing on MappedEncoding" in {
    val expected = WrapEntity(List("hey", "ho").map(StrWrap.apply))
    ctx.run(wrapQ.insert(lift(expected)))
    ctx.run(wrapQ).head mustBe expected
  }

  override protected def beforeEach(): Unit = {
    ctx.run(q.delete)
    ()
  }
}
