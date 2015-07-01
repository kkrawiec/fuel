package scevo.tools

import java.io.File
import java.io.IOException
import java.io.PrintWriter
import java.io.FileOutputStream
import java.io.ObjectOutputStream
import java.io.FileInputStream
import java.io.ObjectInputStream

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

  /* Sequential creation of result files; downside: must check for existenc eof all the previous files 
  val (f, fname) = {
    var i: Int = 0
    var f: File = null
    var fname: String = ""
    try {
      do {
        fname = directory + "/" + filePrefix + fileNumFormat.format(i)
        f = new File(fname + extension)
        i += 1
      } while (!f.createNewFile())
    } catch {
      case e: IOException =>
        throw new IOException(s"Error while creating result database file: " +
          fname + extension + " " + e.getLocalizedMessage());
    }
    (f, fname)
  }
  * 
  */

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

  def saveWorkingState(file: File = f): Unit = {
    val s = new PrintWriter(file)
    toSeq.sortBy(_._1).foreach(kv => s.println(kv._1 + " = " + kv._2))
    s.close()
  }

  def save: Unit = {
    saveWorkingState()
    if (f.canWrite()) f.setReadOnly()
  }

  def saveSnapshot(fnameSuffix: String): Unit =
    saveWorkingState(new File(fname + "." + fnameSuffix))

  def write(key: String, v: Any) = {
    val os = new ObjectOutputStream(new FileOutputStream(fname + "." + key))
    os.writeObject(v)
    os.close()
    v
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