package scevo.util

import scala.tools.reflect.ToolBox
import scala.reflect.runtime.{ currentMirror => cm }

object Combinations {
  // Generates all nonempty n-ary combinations with replacement of elements from elems
  def apply[T](elems: Seq[T], n: Int): Seq[Seq[T]] = {
    assume(n > 0)
    n match {
      case 1 => elems.map(e => Seq(e))
      case _ => elems.flatMap(e => apply(elems, n - 1).map(g => List(e) ++ g))
    }
  }
  // all nonempty combinations
  def apply[T](elems: Seq[T]): Seq[Seq[T]] =
    for (i <- 1 to elems.size) yield apply(elems, i).flatten
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

/**
  * Calls the underlying function f every n calls.
  *
  *  Can be used to make reporting less frequent (e.g., in steady-state EA)
  *
  */
class CallEvery[S](n: Int, f: S => S) extends (S => S) {
  assume(n > 0)
  var i = 0L
  def apply(s: S) = {
    val r = if (i % n == 0) f(s) else s
    i = i + 1
    r
  }
}
object CallEvery {
  def apply[S](n: Int, f: S => S) = new CallEvery(n, f)
}

trait Counter {
  def count: Long
}
class CallCounter[-A, +B](f: A => B) extends (A => B) with Counter {
  private var cnt = 0L
  override def apply(a: A) = {
    cnt = cnt + 1
    f(a)
  }
  override def count = cnt
}
object CallCounter {
  def apply[A, B](f: A => B) = new CallCounter(f)
}

class TrueCounter[-A](f: A => Boolean) extends CallCounter(f){
  private var tcnt = 0L
  override def apply(a: A) = {
    val r = super.apply(a)
    if(r) tcnt = tcnt +1
    r
  }
  def trueCount = tcnt
  def ratio = if(count==0) Double.NaN else tcnt.toDouble/count
}
object TrueCounter {
  def apply[A](f: A => Boolean) = new TrueCounter(f)
}
