package io.getquill.context

import io.getquill.ast._
import io.getquill.idiom.{ Idiom, ReifyStatement, Statement }
import io.getquill.monad.IO

case class Expand[I <: Idiom[_], R](
  idiom:     I,
  ast:       Ast,
  statement: Statement
) {

  val (string, liftings) =
    ReifyStatement(
      idiom.liftingPlaceholder,
      idiom.emptySetContainsToken,
      statement,
      forProbing = false
    )

  /* val prepare =
    (row: Any) => {
      val (_, values, prepare) = liftings.foldLeft((0, List.empty[Any], row)) {
        case ((idx, values, row), lift) =>
          val encoder = lift.encoder.asInstanceOf[context.Encoder[Any]]
          val newRow = encoder(idx, lift.value, row)
          (idx + 1, lift.value :: values, newRow)
      }
      (Nil, row)
    }*/
}

case class ExpandQuery[I <: Idiom[_], R](
  idiom:     I,
  ast:       Ast,
  statement: Statement,
  ct:        Class[R]
) extends IO[List[R]] {
  val (string, liftings) =
    ReifyStatement(
      idiom.liftingPlaceholder,
      idiom.emptySetContainsToken,
      statement,
      forProbing = false
    )
}