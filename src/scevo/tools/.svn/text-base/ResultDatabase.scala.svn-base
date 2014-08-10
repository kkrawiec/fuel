package scevo.tools

import java.io.File
import java.io.IOException
import java.io.PrintWriter

class ResultDatabase(val directory: String) extends scala.collection.mutable.HashMap[String, Any] {

  val extension = ".txt"
  val resultPrefix = "result."
  val filePrefix = "res"
  val fileNumFormat = "%06d"

  val f = {
    var i: Int = 0
    var f: File = null
    try {
      do {
        val fname = directory + "/" + filePrefix + fileNumFormat.format(i) + extension
        f = new File(fname)
        i += 1
      } while (!f.createNewFile());
    } catch {
      case e: IOException => throw new IOException("Error while creating result database file:\n" + e.getLocalizedMessage());
    }
    f
  }

  def fname = f.getPath

  def setResult(key: String, v: Any) = put(resultPrefix + key, v)

  def save: Unit = {
    saveWorkingState
    f.setReadOnly()
  }

  def saveWorkingState: Unit = {
    val s = new PrintWriter(f)
    foreach(kv => s.println(kv._1 + " = " + kv._2))
    s.close()
  }
}