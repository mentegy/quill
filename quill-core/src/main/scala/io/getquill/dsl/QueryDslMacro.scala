package io.getquill.dsl

import io.getquill.util.Messages._
import scala.reflect.macros.blackbox.{ Context => MacroContext }

class QueryDslMacro(val c: MacroContext) {

  import c.universe._

  def expandEntity[T](implicit t: WeakTypeTag[T]): Tree = {
    val res = q"${meta[T]("Schema")}.entity"
    res
  }

  def expandInsert[T](value: Tree)(implicit t: WeakTypeTag[T]): Tree =
    expandAction(value, "Insert")

  def expandUpdate[T](value: Tree)(implicit t: WeakTypeTag[T]): Tree =
    expandAction(value, "Update")

  private def expandAction[T](value: Tree, prefix: String)(implicit t: WeakTypeTag[T]) =
    q"${meta(prefix)}.expand(${c.prefix}, $value)"

  private def meta[T](prefix: String)(implicit t: WeakTypeTag[T]): Tree = {
    val tree = tq"io.getquill.dsl.IdiomMetaDsl#${TypeName(s"${prefix}Meta")}[$t]"
    println(s"meta for $tree")
    val expanderTpe = c.typecheck(tree, c.TYPEmode)
    c.inferImplicitValue(expanderTpe.tpe) match {
      case EmptyTree => c.fail(s"Can't find an implicit `${prefix}Meta` for type `${t.tpe}`")
      case tree      => tree
    }
  }
}
