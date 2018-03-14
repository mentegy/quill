package io.getquill.source


import scala.reflect.macros.whitebox.{Context => MacroContext}

class SourceMacro(val c: MacroContext) {

  import c.universe._

  def expandApply[T](io: Tree)(implicit t: WeakTypeTag[T]): Tree = {


    // TODO
    // THis will not work
    // pre-define supported types in idiom
    // implement supported types in sources
    // other types => requires encoders
    q"""
       val res = io.getquill.monad.TransformIO(io) {
         case e: io.getquill.context.ExpandQuery[_, _] => ${materializeExpandQuery(q"e")}
       }
       ${c.prefix}.unsafePerformIo(res)
     """
  }


  private def materializeExpandQuery(e: Tree): Tree = {
    q"""
       ${c.prefix}.Run(() => ${c.prefix}.executeQuery($e.string, ${prepare(e)}, ${extract(e)}))
     """
  }


  private def prepare(expand: Tree): Tree = {

  }


  private def extract(expand: Tree): Tree = ???

}


