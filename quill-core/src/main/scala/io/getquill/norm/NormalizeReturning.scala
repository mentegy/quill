package io.getquill.norm

import io.getquill.ast._

object NormalizeReturning {

  def apply(e: Action): Action = {
    e match {
      case Returning(a: Action, alias, body) => Returning(apply(a, body), alias, body)
      case _                                 => e
    }
  }

  private def apply(e: Action, body: Ast): Action = e match {
    case Insert(query, assignments)       => Insert(query, filterReturnedColumn(assignments, body))
    case Update(query, assignments)       => Update(query, filterReturnedColumn(assignments, body))
    case Conflict(a: Action, target, act) => Conflict(apply(a, body), target, act)
    case _                                => e
  }

  private def filterReturnedColumn(assignments: List[Assignment], column: Ast): List[Assignment] =
    assignments.flatMap(filterReturnedColumn(_, column))

  private def filterReturnedColumn(assignment: Assignment, column: Ast): Option[Assignment] =
    (assignment, column) match {
      case (Assignment(_, Property(_, p1), _), Property(_, p2)) if (p1 == p2) =>
        None
      case (assignment, column) =>
        Some(assignment)
    }
}
