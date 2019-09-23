package fuel.util

import scala.tools.reflect.ToolBox
import scala.reflect.runtime.{ currentMirror => cm }
import scala.reflect.runtime.{currentMirror => cm}

object Combinations {
  // Generates all nonempty n-ary combinations with replacement of elements from elems
  def apply[T](elems: Seq[T], n: Int): Seq[Seq[T]] = {
    assert(n > 0)
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
  * Every n calls, performs a calls to function f.
  *
  *  Can be used to make reporting less frequent (e.g., in steady-state EA)
  *
  */
class CallEvery[S](n: Int, f: S => S) extends (S => S) {
  assert(n > 0)
  private[this] var i = 0L
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
  def reset(): Unit
}
class CallCounter[-A, +B](f: A => B) extends (A => B) with Counter {
  private[this] var cnt = 0L
  override def apply(a: A) = {
    cnt = cnt + 1
    f(a)
  }
  override def reset(): Unit = cnt = 0L
  override def count: Long = cnt
}
object CallCounter {
  def apply[A, B](f: A => B) = new CallCounter(f)
}

class TrueCounter[-A](f: A => Boolean) extends CallCounter(f){
  private[this] var tcnt = 0L
  override def apply(a: A) = {
    val r = super.apply(a)
    if(r) tcnt = tcnt +1
    r
  }
  override def reset(): Unit = tcnt = 0L
  def trueCount: Long = tcnt
  def ratio: Double = if (count==0) Double.NaN else tcnt.toDouble/count
}
object TrueCounter {
  def apply[A](f: A => Boolean) = new TrueCounter(f)
}


class ParseErrorException(message: String = "", cause: Throwable = null)
  extends Exception(message, cause)

object Parsers {
  def parsePropertiesFile(text: String): Options = {
    val m = text.split("\n").collect{ case line if line.trim.nonEmpty =>
      if (!line.contains('='))
        throw new ParseErrorException("Wrong format of the properties file.")
      val pos = line.indexOf('=')
      val (name, value) = line.splitAt(pos)
      (name.trim, value.drop(1).trim)
    }.toMap
    new OptionsMap(m)
  }
}


object Utils {
  /**
    * Simple median algorithm based on sorting.
    */
  def median(seq: Seq[Double], isSorted: Boolean = false): Double = {
    //In order if you are not sure that 'seq' is sorted
    if (isSorted) {
      if (seq.size % 2 == 1) seq(seq.size / 2)
      else {
         //val (up, down) = seq.splitAt(seq.size / 2)
         //(up.last + down.head) / 2
        (seq(seq.size / 2 - 1) + seq(seq.size / 2)) / 2
      }
    }
    else
      median(seq.sortWith(_ < _), isSorted=true)
  }
}