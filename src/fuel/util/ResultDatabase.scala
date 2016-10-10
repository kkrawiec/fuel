package fuel.util

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
  * Intended to be created at the beginning of experiment. 
  * It creates a new result file with a given name, or with a random file name. 
  */
class ResultDatabase(val directory: String, fileName: String = "")
    extends scala.collection.mutable.HashMap[String, Any] {

  val extension = ".txt"
  val resultPrefix = "result."
  val filePrefix = "res"

  val (f, fname) = if (fileName != "") {
    val fullName = directory + File.separator + fileName
    (new File(fullName), fullName)
  } else {
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
  def getResult(key: String) = get(resultPrefix + key)

  def save(file: File = f): Unit = {
    val s = new PrintWriter(file)
    toSeq.sortBy(_._1).foreach(kv => s.println(kv._1 + " = " + kv._2))
    s.close()
  }
  override def toString = toSeq.sortBy(_._1).map(kv => s"${kv._1} = ${kv._2}").mkString("\n")

  def saveSnapshot(fnameSuffix: String): Unit =
    save(new File(fname + "." + fnameSuffix))

    /** Serializes an object to a file if it's serializable; otherwise saves its 
     *  textual representation. 
     */
  def write(key: String, v: Any) = {
    val fn = fname + "." + key
    val os = new ObjectOutputStream(new FileOutputStream(fn))
    try {
      os.writeObject(v)
      os.close()
    } catch {
      case e: NotSerializableException => {
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