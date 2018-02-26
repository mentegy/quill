package test

import io.getquill._

case class CityId(id: Int) extends AnyVal
case class City(id: CityId, name: String, population: Int, area: Float, link: Option[String])

case class MetroSystemId(id: Int) extends AnyVal
case class MetroSystem(id: MetroSystemId, cityId: CityId, name: String, dailyRidership: Int)

case class MetroLineId(id: Int) extends AnyVal
case class MetroLine(id: MetroLineId, systemId: MetroSystemId, name: String, stationCount: Int)

object Test {
  def main(args: Array[String]): Unit = {
    val ctx = new SqlMirrorContext(PostgresDialect, SnakeCase)

    import ctx._

    case class MetroSystemWithLineCount(metroSystemName: String, cityName: String, lineCount: Long)

    val q1 = quote {
      (for {
        (ml, ms) <- query[MetroLine].join(query[MetroSystem]).on(_.systemId == _.id)
        c <- query[City].join(_.id == ms.cityId)
      } yield (ml, ms, c))
        .groupBy {
          case (ml, ms, c) =>
            (ms.id, c.id, ms.name, c.name)
        }
        .map {
          case ((msId, cId, msName, cName), aggregated) =>
            (msName, cName, aggregated.size)
        }
    }

    println(ctx.run(q1.dynamic).string)

    /*val q = quote {
      for {
        ((ml, ms), c) <- query[MetroLine]
          .join(query[MetroSystem])
          .on(_.systemId == _.id)
          .join(query[City])
          .on(_._2.cityId == _.id)
      } yield (ms.id, ms.name, c.name, c.id)
    }

    println(ctx.run(q.nested.map({ case (id1, _, _, id2) => (id1, id2) }).dynamic).string)*/

    /*case class T1(x: Int, y: Int)
    case class T2(x: Int, y: Int)

    ctx.run {
      (for {
        t1 <- query[T1]
        t2 <- query[T2]
      } yield {
        (t1.x, t2.x, t1.y, t2.y)
      }).nested
        .map {
          case (x1, x2, y1, y2) => (x1, x2, y1)
        }
    }*/

    /*case class Person(name: String, age: Int)

    val m = ctx.run(query[Person].groupBy(_.age).map {
      case (age, people) => (age, people.size + 1)
    }.nested)

    println(m.string)*/

    ()
  }
}