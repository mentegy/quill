package io.getquill.context.async.postgres

import java.time.LocalDate

import io.getquill.context.sql.dsl.ArrayEncodingSpec
import org.joda.time.{ LocalDate => JodaLocalDate, LocalDateTime => JodaLocalDateTime }

import scala.concurrent.ExecutionContext.Implicits.global

class ArrayAsyncEncodingSpec extends ArrayEncodingSpec {
  val ctx = testContext
  import ctx._

  "Support all sql base types and `Traversable` implementers" in {
    await(ctx.run(q.insert(lift(e))))
    val actual = await(ctx.run(q)).head
    actual mustEqual e
    baseEntityDeepCheck(actual, e)
  }

  "Joda times" in {
    case class JodaTimes(timestamps: Seq[JodaLocalDateTime], dates: Seq[JodaLocalDate])
    val jE = JodaTimes(Seq(JodaLocalDateTime.now()), Seq(JodaLocalDate.now()))
    val jQ = quote(querySchema[JodaTimes]("ArraysTestEntity"))
    await(ctx.run(jQ.insert(lift(jE))))
    val actual = await(ctx.run(jQ)).head
    actual.timestamps mustBe jE.timestamps
    actual.dates mustBe jE.dates
  }

  implicit val strWrapEncode: MappedEncoding[StrWrap, String] = MappedEncoding(_.str)
  implicit val strWrapDecode: MappedEncoding[String, StrWrap] = MappedEncoding(StrWrap.apply)
  "Support Traversable encoding basing on MappedEncoding" in {
    await(ctx.run(wrapQ.insert(lift(wrapE))))
    await(ctx.run(wrapQ)).head mustBe wrapE
  }

  "Catch invalid decoders" in {
    val newCtx = new TestContext {
      // avoid transforming from org.joda.time.LocalDate to java.time.LocalDate
      override implicit def arrayLocalDateDecoder[Col <: Traversable[LocalDate]](implicit bf: CBF[LocalDate, Col]): Decoder[Col] =
        arrayDecoder[LocalDate, LocalDate, Col](identity)
    }
    import newCtx._
    await(newCtx.run(query[ArraysTestEntity].insert(lift(e))))
    intercept[IllegalStateException] {
      await(newCtx.run(query[ArraysTestEntity])).head mustBe e
    }
    newCtx.close()
  }

  override protected def beforeEach(): Unit = {
    await(ctx.run(q.delete))
    ()
  }
}
