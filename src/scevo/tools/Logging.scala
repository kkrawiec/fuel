package scevo.tools

import scevo.evo.IterativeAlgorithm
import scevo.evo.Experiment
import scevo.evo.State

/*
trait Logging {

  var log = List[String]()
  def log(s: String) {
    println(s)
    log = log :+ s
  }
}
* 
*/

trait Closeable {
  protected def close = {}
}

// Note: passing arguments by name, so that they get evaluated only when needed
trait Logger {
  def log(key: => Any, value: => Any) = {}
}

trait LoggerIter extends Logger with Closeable {
  this: Experiment[_ <: State] with IterativeAlgorithm[_ <: State] =>

  val stats = scala.collection.mutable.HashMap[(Int, Any), Any]() // (iteration, key) => val
  override def log(key: => Any, value: => Any) = {
    stats += (((currentState.iteration, key), value))
  }
  override protected def close = {
    super.close
    val keys = stats.keys.map(_._2).toSet
    keys.foreach(key => {
      val v = stats.filter(e => e._1._2 == key).map(e => (e._1._1, e._2))
      val m = scala.tools.nsc.io.File(rdb.fname + f".$key").writeAll(
        v.toList.sortBy(_._1).map(e => f"${e._1}\t${e._2}").mkString("\n"))
    })
  }
}