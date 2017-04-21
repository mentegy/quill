package io.getquill.dsl

import io.getquill.util.OptionalTypecheck
import io.getquill.util.Messages._
import scala.reflect.macros.blackbox.{ Context => MacroContext }

class EncodingDslMacro(val c: MacroContext) {
  import c.universe._

  def materializeEncoder[T](implicit t: WeakTypeTag[T]): Tree =
    anyValEncoder(t.tpe)
      .getOrElse(fail("Encoder", t.tpe))

  def materializeDecoder[T](implicit t: WeakTypeTag[T]): Tree =
    anyValDecoder(t.tpe)
      .getOrElse(fail("Decoder", t.tpe))

  def traversableEncoder[I, O, Col <: Traversable[I]](implicit iTag: WeakTypeTag[I], oTag: WeakTypeTag[O],
                                                      cTag: WeakTypeTag[Col]): Tree = {
    OptionalTypecheck(c)(q"implicitly[${c.prefix}.MappedEncoding[$oTag, $iTag]]") match {
      case Some(mapped) =>
        OptionalTypecheck(c)(q"implicitly[${c.prefix}.Encoder[Traversable[$oTag]]]") match {
          case None => fail("Encoder", cTag.tpe)
          case Some(e) =>
            q"""
              ${c.prefix}.mappedEncoder[$cTag, Traversable[$iTag]](
                io.getquill.MappedEncoding((col: $cTag) => col.map($mapped.f)), $e
              )
            """
        }
      case None => fail("Encoder", cTag.tpe)
    }
  }

  def traversableDecoder[I, O, Col <: Traversable[O]](implicit iTag: WeakTypeTag[I], oTag: WeakTypeTag[O],
                                                      cTag: WeakTypeTag[Col]): Tree = {
    OptionalTypecheck(c)(q"implicitly[${c.prefix}.MappedEncoding[$iTag, $oTag]]") match {
      case Some(mapped) =>
        OptionalTypecheck(c)(q"implicitly[${c.prefix}.Decoder[Traversable[$iTag]]]") match {
          case None => fail("Decoder", cTag.tpe)
          case Some(e) =>
            q"""
              val bf = scala.collection.generic.CanBuildFrom[Nothing, $oTag, $cTag]
              ${c.prefix}.mappedDecoder[Traversable[$iTag], $cTag](io.getquill.MappedEncoding(
                (t: Traversable[$iTag]) => t.foldLeft(bf())((b, x) => b += $mapped.f(x)).result()), $e
              )
            """
        }
      case None => fail("Decoder", cTag.tpe)
    }
  }

  def lift[T](v: Tree)(implicit t: WeakTypeTag[T]): Tree =
    lift[T](v, "lift")

  def liftQuery[T](v: Tree)(implicit t: WeakTypeTag[T]): Tree =
    lift[T](v, "liftQuery")

  private def lift[T](v: Tree, method: String)(implicit t: WeakTypeTag[T]): Tree =
    OptionalTypecheck(c)(q"implicitly[${c.prefix}.Encoder[$t]]") match {
      case Some(enc) =>
        q"${c.prefix}.${TermName(s"${method}Scalar")}($v)($enc)"
      case None =>
        t.tpe.baseType(c.symbolOf[Product]) match {
          case NoType => fail("Encoder", t.tpe)
          case _ =>
            q"${c.prefix}.${TermName(s"${method}CaseClass")}($v)"
        }
    }

  private def fail(enc: String, t: Type) =
    c.fail(s"Can't find $enc for type '$t'")

  private def anyValDecoder(tpe: Type): Option[Tree] =
    withAnyValParam(tpe) { param =>
      q"""
        ${c.prefix}.mappedDecoder(
          io.getquill.MappedEncoding((p: ${param.typeSignature}) => new $tpe(p)),
          implicitly[${c.prefix}.Decoder[${param.typeSignature}]]
        )
      """
    }

  private def anyValEncoder(tpe: Type): Option[Tree] =
    withAnyValParam(tpe) { param =>
      q"""
        ${c.prefix}.mappedEncoder(
          io.getquill.MappedEncoding((v: $tpe) => v.${param.name.toTermName}),
          implicitly[${c.prefix}.Encoder[${param.typeSignature}]]
        )
      """
    }

  private def withAnyValParam[R](tpe: Type)(f: Symbol => R): Option[R] =
    tpe.baseType(c.symbolOf[AnyVal]) match {
      case NoType => None
      case _ =>
        primaryConstructor(tpe).map(_.paramLists.flatten).collect {
          case param :: Nil => f(param)
        }
    }

  private def primaryConstructor(t: Type) =
    t.members.collect {
      case m: MethodSymbol if m.isPrimaryConstructor => m.typeSignature.asSeenFrom(t, t.typeSymbol)
    }.headOption
}
