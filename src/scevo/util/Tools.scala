package scevo.util

import scala.tools.reflect.ToolBox
import scala.reflect.runtime.{currentMirror => cm}

object Combinations {
  // Generates all n-ary combinations with replacement of elements from elems
  def apply[T](elems: Seq[T], n: Int): Seq[Seq[T]] = {
    require(n > 0)
    n match {
      case 1 => elems.map(e => Seq(e))
      case _ => elems.flatMap(e => apply(elems, n - 1).map(g => List(e) ++ g))
    }
  }
  def apply(m: Int, n: Int): Seq[Seq[Int]] = apply(Seq.range(0, m), n)
}

object CodeExecutor {
  import scala.reflect.runtime.{ currentMirror => cm }
  def apply(code: String) = {
    try {
      val toolBox = cm.mkToolBox()
      val ast = toolBox.parse(code)
      toolBox.compile(ast)()
    } catch {
      case e: Throwable => throw new Exception(
        "Error when parsing/compiling/executing Scala code: " + code, e)
    }
  }
}

