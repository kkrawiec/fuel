package scevo.tools

import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.NotSerializableException
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.io.PrintWriter

/**
  * A container for writing intermediate and final results to a file. 
  * 
  * Intended to be created at the beginning of experiment, when it creates a new result file
  * with a unique name. 
  */
class ResultDatabase(val directory: String) extends scala.collection.mutable.HashMap[String, Any] {

  val extension = ".txt"
  val resultPrefix = "result."
  val filePrefix = "res"
  val fileNumFormat = "%06d"

  val (f, fname) = {
    var f: File = null
    try {
      f = File.createTempFile(filePrefix, extension, new File(directory))
    } catch {
      case e: Exception =>
        throw new Exception(s"Error while creating result database file. "
          + e.getLocalizedMessage());
    }
    (f, f.getCanonicalFile())
  }

  def setResult(key: String, v: Any) = put(resultPrefix + key, v)

  def save(file: File = f): Unit = {
    val s = new PrintWriter(file)
    toSeq.sortBy(_._1).foreach(kv => s.println(kv._1 + " = " + kv._2))
    s.close()
  }

  def saveSnapshot(fnameSuffix: String): Unit =
    save(new File(fname + "." + fnameSuffix))

  def write(key: String, v: Any) = {
    val fn = fname + "." + key
    val os = new ObjectOutputStream(new FileOutputStream(fn))
    try {
      os.writeObject(v)
      os.close()
    } catch {
      case e: NotSerializableException => {
        // TODO: Warning about non-serializable object?
        // TODO: Opens the same file, but still works - os is probably already closed here?
        val s = new PrintWriter(new File(fn))
        s.println(v.toString)
        s.close
      }
    } finally {
      os.close
    }
  }
  def writeString(key: String, v: String) = {
    val s = new PrintWriter(new File(fname + "." + key))
    s.print(v)
    s.close()
    v
  }

  def read(key: String) = {
    val is = new ObjectInputStream(new FileInputStream(fname + "." + key))
    val obj = is.readObject()
    is.close()
    obj
  }
}