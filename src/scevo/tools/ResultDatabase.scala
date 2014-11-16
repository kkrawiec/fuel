package scevo.tools

import java.io.File
import java.io.IOException
import java.io.PrintWriter

/**
  * A container for storing intermediate and final results.
  * Intended to be created at the beginning of experiment, when it creates a new result file
  * with a unique, consecutive name like res000001.txt
  */
class ResultDatabase(val directory: String) extends scala.collection.mutable.HashMap[String, Any] {

  val extension = ".txt"
  val resultPrefix = "result."
  val filePrefix = "res"
  val fileNumFormat = "%06d"

  val f = {
    var i: Int = 0
    var f: File = null
      var fname : String = ""
    try {
      do {
        fname = directory + "/" + filePrefix + fileNumFormat.format(i) + extension
        f = new File(fname)
        i += 1
      } while (!f.createNewFile())
    } catch {
      case e: IOException => 
        throw new IOException(s"Error while creating result database file: "+fname + " " + e.getLocalizedMessage());
    }
    f
  }

  def fname = f.getPath

  def setResult(key: String, v: Any) = put(resultPrefix + key, v)

  def saveWorkingState(file: File = f): Unit = {
    val s = new PrintWriter(f)
    toSeq.sortBy(_._1).foreach(kv => s.println(kv._1 + " = " + kv._2))
    s.close()
  }

  def save: Unit = {
    saveWorkingState()
    if (f.canWrite()) f.setReadOnly()
  }

  def saveSnapshot(fnameSuffix: String): Unit =
    saveWorkingState(new File(f.getPath() + fnameSuffix))

  override def finalize(): Unit = save

}