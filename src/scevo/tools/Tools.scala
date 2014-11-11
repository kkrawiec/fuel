package scevo.tools

import scala.tools.reflect.ToolBox

object Combinations {

  // Generates all n-ary combinations of elements from elems
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
      /*
      val cl = getClass.getClassLoader
      val toolBox = runtimeMirror(cl).mkToolBox()
      */
      // TODO: Verify: This looks simpler, so maybe its faster: 
      val toolBox = cm.mkToolBox()
      val ast = toolBox.parse(code)
      toolBox.compile(ast)()
    } catch {
      case e: Throwable => throw new Exception(
        "Error when parsing/compiling/executing Scala code: " + code, e)
    }
  }
}

object Stats {

  def descriptive(l: Seq[Double]) = {
    require(l.nonEmpty)
    (l.min, l.sum / l.size, l.max)
  }

  def basic(l: Seq[Double]) = {
    val (sMin, sMean, sMax) = descriptive(l)
    f"Min: $sMin  Avg: $sMean%.2f  Max: $sMax"
  }
}