package io.getquill.norm

import io.getquill.ast._

object SymbolicReduction {

  def unapply(q: Query) =
    q match {

      // a.filter(b => c).flatMap(d => e.$) =>
      //     a.flatMap(d => e.filter(_ => c[b := d]).$)
      case FlatMap(Filter(a, b, c), d, e: Query) =>
        val cr = BetaReduction(c, b -> d)
        val er = AttachToEntity(Filter(_, _, cr))(e)
        Some(FlatMap(a, d, er))

      /* case FlatMap(Distinct(Map(a, b, p: Property)), i, e: Query) =>
        val pr = BetaReduction(p, b -> i)
        BetaReduction(e, i -> pr) match {
          case `e` => None
          case er  => Some(FlatMap(Distinct(Map(a, b, p)), i, er))
        }*/
      // a.flatMap(b => c).flatMap(d => e) =>
      //     a.flatMap(b => c.flatMap(d => e))
      case FlatMap(FlatMap(a, b, c), d, e) =>
        Some(FlatMap(a, b, FlatMap(c, d, e)))

      // a.union(b).flatMap(c => d)
      //      a.flatMap(c => d).union(b.flatMap(c => d))
      case FlatMap(Union(a, b), c, d) =>
        Some(Union(FlatMap(a, c, d), FlatMap(b, c, d)))

      // a.unionAll(b).flatMap(c => d)
      //      a.flatMap(c => d).unionAll(b.flatMap(c => d))
      case FlatMap(UnionAll(a, b), c, d) =>
        Some(UnionAll(FlatMap(a, c, d), FlatMap(b, c, d)))

      case other => None
    }
}
