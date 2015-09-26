package scevo.tools

import scala.reflect.io.Path.string2path

/* To be implemented by the components that need to be closed before program termination
 * 
 */
trait Closeable {
  def close = {}
}

// Note: passing arguments by name, so that they get evaluated only when needed
/*
trait Logger[K] {
  def log(key: => K, value: => Any): Logger[K]
}

trait LoggerIter extends Logger[(Int, Any)] with Closeable {
  this: Collector =>

  val stats = scala.collection.mutable.HashMap[(Int, Any), Any]() 

  override def log(key: => (Int, Any), value: => Any) = {
    stats += ((key, value))
    this
  }

  override def close = {
    val keys = stats.keys.map(_._2).toSet
    keys.foreach(key => {
      val v = stats.filter(e => e._1._2 == key).map(e => (e._1._1, e._2))
      val m = scala.tools.nsc.io.File(rdb.fname + f".$key").writeAll(
        v.toList.sortBy(_._1).map(e => f"${e._1}\t${e._2}").mkString("\n"))
    })
    super.close
  }
}
*/