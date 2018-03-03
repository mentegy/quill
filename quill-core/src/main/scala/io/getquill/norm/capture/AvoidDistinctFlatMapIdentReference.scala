package io.getquill.norm.capture

import io.getquill.ast._
import io.getquill.norm.BetaReduction

object AvoidDistinctFlatMapIdentReference extends StatelessTransformer {
  override def apply(q: Query): Query = {
    q match {
      case f @ FlatMap(Distinct(Map(a, b, p: Property)), i, e: Query) =>
        val pr = BetaReduction(p, b -> i)
        CollectAst(e) {
          case `pr` => pr
        } match {
          case Nil =>
            val er = BetaReduction(e, i -> pr)
            FlatMap(Distinct(Map(a, b, p)), i, er)
          case _ =>
            f
        }

      case _ =>
        super.apply(q)
    }
  }
}