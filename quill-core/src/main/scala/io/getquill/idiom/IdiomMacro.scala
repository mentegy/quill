package io.getquill.idiom


import scala.reflect.macros.whitebox.Context
import io.getquill.ast.Ast
import io.getquill.ast.Dynamic
import io.getquill.quotation.Quotation
import io.getquill.util.Messages._
import io.getquill.quotation.IsDynamic
import io.getquill.ast.Lift
import io.getquill.NamingStrategy
import io.getquill.context.{ProbeStatement, VerifyFreeVariables}

import scala.util.Success
import scala.util.Failure

trait IdiomMacro extends Quotation {
  val c: Context
  import c.universe.{ Function => _, Ident => _, _ }

  protected def expand[T](ast: Ast, t: WeakTypeTag[T]): Tree = {

    val tree = c.typecheck {
      q"""
        val (ast, statement) = ${translate(ast)}
        io.getquill.context.ExpandQuery(${c.prefix}, ast, statement, ${c.reifyRuntimeClass(t.tpe)})
      """
    }

    println(s"expand:\n$tree")

    tree
  }

  protected def extractAst[T](quoted: Tree): Ast =
    unquote[Ast](c.typecheck(q"quote($quoted)"))
      .map(VerifyFreeVariables(c))
      .getOrElse {
        Dynamic(quoted)
      }

  protected def translate(ast: Ast): Tree = {
    val isDyn = IsDynamic(ast)
    println(s"IS DYNAMIC: " + isDyn)
    println(ast)
    IsDynamic(ast) match {
      case false => translateStatic(ast)
      case true  => translateDynamic(ast)
    }
  }

  private implicit val tokenLiftable: Liftable[Token] = Liftable[Token] {
    case StringToken(string)        => q"io.getquill.idiom.StringToken($string)"
    case ScalarLiftToken(lift)      => q"io.getquill.idiom.ScalarLiftToken(${lift: Lift})"
    case Statement(tokens)          => q"io.getquill.idiom.Statement(scala.List(..$tokens))"
    case SetContainsToken(a, op, b) => q"io.getquill.idiom.SetContainsToken($a, $op, $b)"
  }

  private def translateStatic(ast: Ast): Tree = {
    idiomAndNamingStatic match {
      case Success(idiom) =>
        val (normalizedAst, statement) = idiom.translate(ast)

        val (string, _) =
          ReifyStatement(
            idiom.liftingPlaceholder,
            idiom.emptySetContainsToken,
            statement,
            forProbing = true
          )

        ProbeStatement(idiom.prepareForProbing(string), c)

        c.info(string)

        val res = q"($normalizedAst, ${statement: Token})"
        println(s"CONTEXTMACRO: " + res)
        res
      case Failure(ex) =>
        c.info(s"Can't translate query at compile time because the idiom and/or the naming strategy aren't known at this point.")
        ex.printStackTrace()
        translateDynamic(ast)
    }
  }

  private def translateDynamic(ast: Ast): Tree = {
    c.info("Dynamic query")
    q"""
      ${c.prefix}.translate($ast)
    """
  }

  private def idiomAndNaming = {
    val (n :: _) =
      c.prefix.actualType
        .baseType(c.weakTypeOf[Idiom[NamingStrategy]].typeSymbol)
        .typeArgs
    n
  }

  private def idiomAndNamingStatic =
    for {
      naming <- LoadNaming.static(c)(idiomAndNaming)
      idiom <- loadIdiom(naming, c.prefix.actualType)
    } yield {
      idiom
    }
  private def loadIdiom(naming: NamingStrategy, tpe: Type) = scala.util.Try {
    val cls = Class.forName(tpe.typeSymbol.fullName)
    val cns = cls.getConstructor(classOf[NamingStrategy])
    cns.newInstance(naming).asInstanceOf[Idiom[NamingStrategy]]
  }
}
