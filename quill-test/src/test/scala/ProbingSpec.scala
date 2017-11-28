import io.getquill.Spec

class ProbingSpec extends Spec {
  case class TestEntity(i: Int)
  val e = TestEntity(1)
  val i = 1

  "mysql" in {
    val c = io.getquill.context.jdbc.MysqlJdbcQueryProbing
    import c._
    c.run(query[TestEntity].filter(_.i == lift(i)))
    c.run(query[TestEntity].insert(lift(e)))
  }

  "postgres" in {
    val c = io.getquill.context.jdbc.PostgresJdbcQueryProbing

    import c._

    c.run(query[TestEntity].filter(_.i == lift(i)))
    c.run(query[TestEntity].insert(lift(e)))
  }
}
