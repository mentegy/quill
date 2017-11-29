package quilltests

import io.getquill.{ PostgresDialect, SnakeCase, SqlMirrorContext }

object UpsertTest {
  val ctx = new SqlMirrorContext(PostgresDialect, SnakeCase)
  import ctx._

  case class Tweet(tweetId: Int, tweetTimestamp: Long, tweetContent: String, tweetName: String)
  val tweet = Tweet(1, 123, "content", "y")
  // val ins1 = quote(query[Tweet].insert(lift(tweet)))
  //val ins2 = quote(query[Tweet].insert(_.tweetContent -> "hee"))

  //run(ins.onConflictDoNothing)
  //run(ins.onConflictDoNothing("const_twe"))
  run(query[Tweet].insert(_.tweetContent -> "hee")
    .onConflictDoUpdate("const")(_.tweetContent -> excluded[Tweet].tweetName))
  //run(query[Tweet].update(_.tweetId -> 1, _.tweetContent -> lift("yay")))
}
