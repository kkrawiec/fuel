package fuel.util

import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.io.NotSerializableException
import java.io.ObjectInputStream
import java.io.ObjectOutputStream
import java.io.PrintWriter

trait ResultDatabase extends scala.collection.mutable.HashMap[String, Any] {
  val resultPrefix = "result."
  def setResult(key: String, v: Any): Unit
  def getResult(key: String): Option[Any]
  def write(key: String, v: Any): Unit
  def writeString(key: String, v: String): Unit
  def read(key: String): Object
  def deleteSnapshot(fnameSuffix: String): Unit
  def saveSnapshot(fnameSuffix: String): Unit
  def save(): Unit
  def deleteArtifacts(): Unit
}

/**
  * A container for writing intermediate and final results to a file.
  *
  * Intended to be created at the beginning of experiment. 
  * It creates a new result file with a given name, or with a random file name. 
  */
class ResultDatabaseFile(val directory: String, fileName: String = "") extends ResultDatabase {
  val extension = ".txt"
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

  def save(file: File): Unit = {
    val s = new PrintWriter(file)
    toSeq.sortBy(_._1).foreach(kv => s.println(kv._1 + " = " + kv._2))
    s.close()
  }
  def save() = save(f)
  override def toString = toSeq.sortBy(_._1).map(kv => s"${kv._1} = ${kv._2}").mkString("\n")

  def deleteSnapshot(fnameSuffix: String): Unit =
    new File(fname + "." + fnameSuffix).delete()

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
    //v  // commented out to be consistent with write.
  }

  def read(key: String) = {
    val is = new ObjectInputStream(new FileInputStream(fname + "." + key))
    val obj = is.readObject()
    is.close()
    obj
  }
  
  def deleteArtifacts() = f.delete()
}


/**
  * A container for only storing evolution data during runtime.
  * Any saving of stored values will result in printing them to the standard output.
  * Use this container if you don't want to have any artifacts saved on the hard disk.
  *
  * Intended to be created at the beginning of experiment.
  */
class ResultDatabasePlain() extends ResultDatabase {
  def setResult(key: String, v: Any) = put(resultPrefix + key, v)
  def getResult(key: String) = get(resultPrefix + key)
  def write(key: String, v: Any) = println(key + " = " + v)
  def writeString(key: String, v: String) = println(key + " = " + v)
  def read(key: String) = ???
  def deleteSnapshot(fnameSuffix: String): Unit = { /* do nothing */ }
  def saveSnapshot(fnameSuffix: String): Unit = {
    println(fnameSuffix)
    this.toList.sortBy(_._1).foreach{case (k, v) => println(k + " = " + v)}
  }
  def save() = saveSnapshot("\nCOLLECTED DATA:")
  def deleteArtifacts(): Unit = {}
}