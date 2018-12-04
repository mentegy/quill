
import io.getquill._

object Test {

  case class Person(id: Int, name: String)

  def main(args: Array[String]): Unit = {
    val ctx = new MirrorContext(MirrorIdiom, Literal)

    import ctx._

    val q = quote {
      query[Person].filter(p => p.id > 0).sortBy(_.name)
    }

    val r = run(q)

    println(r.string)
  }

}