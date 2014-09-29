package scevo.tools

import scala.reflect.runtime.universe._
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
  def apply(code: String) = {
    try {
      val toolBox = runtimeMirror(getClass.getClassLoader).mkToolBox()
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
object Entropy {
  def bothWays[T, U](x: Seq[T], y: Seq[U]) = {
    require(x.size == y.size)
    val n = x.size.toDouble
    def freq[T](l: Seq[T]) = l.groupBy(v => v).map(e => (e._1, e._2.size))
    val margx: Map[T, Int] = freq(x)
    val margy: Map[U, Int] = freq(y)
    val joint: Map[(T, U), Int] = freq(x.zip(y))
    (for ((pair: (T, U), cnt: Int) <- joint) yield {
      val jointProb = cnt / n
      jointProb * (math.log(margx(pair._1).toDouble / cnt) + math.log(margy(pair._2).toDouble / cnt))
    }).sum
  }
  //    joint.foreach( {case (pair:(T,U),cnt:Int) => 
  def main(args: Array[String]) {
    println(bothWays[Int, Int](Seq(1, 2, 2, 3, 4, 4), Seq(3, 3, 0, 3, 4, 4)))
    println(bothWays[Int, Int](Seq(1, 2, 2, 3, 4, 4), Seq(3, 0, 0, 8, 2, 2)))
    println(bothWays[Int, Int](Seq(1, 1, 1, 1, 1, 1), Seq(3, 0, 0, 8, 2, 2)))
    println(bothWays[Int, Int](0 until 7, 0 until 7))
  }
}
