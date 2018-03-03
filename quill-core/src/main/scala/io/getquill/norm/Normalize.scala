package io.getquill.norm

import io.getquill.ast.Ast
import io.getquill.ast.Query
import io.getquill.ast.StatelessTransformer
import io.getquill.norm.capture.{ AvoidCapture, AvoidDistinctFlatMapIdentReference }
import io.getquill.ast.Action

import scala.annotation.tailrec

object Normalize extends StatelessTransformer {

  override def apply(q: Ast): Ast =
    super.apply(BetaReduction(q))

  override def apply(q: Action): Action =
    NormalizeReturning(super.apply(q))

  override def apply(q: Query): Query =
    norm(AvoidDistinctFlatMapIdentReference(AvoidCapture(q)))

  @tailrec
  private def norm(q: Query): Query =
    q match {
      case NormalizeNestedStructures(query) =>
        println(s"NormalizeNestedStructures: \n 		$q -> \n 		$query")
        norm(query)
      case ApplyMap(query) =>
        println(s"ApplyMap: \n 		$q -> \n 		$query")
        norm(query)
      case SymbolicReduction(query) =>
        println(s"SymbolicReduction: \n 		$q -> \n 		$query")
        norm(query)
      case AdHocReduction(query) =>
        println(s"AdHocReduction: \n 		$q -> \n 		$query")
        norm(query)
      case OrderTerms(query) =>
        println(s"OrderTerms: \n 		$q -> \n 		$query")
        norm(query)
      case other => other
    }
}
