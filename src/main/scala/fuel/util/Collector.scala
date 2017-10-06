package fuel.util

import java.lang.management.ManagementFactory
import java.net.InetAddress
import java.net.UnknownHostException

/**
  * Collector of results. Each entry is a pair (key,value).
  *
  * setResult() should be used for the actual results for experiment, i.e., the
  * 'essential' reporting. set() is more for technical reporting (e.g., timing,
  * location, files, etc.)
  *
  */
trait Collector extends Closeable {
  def rdb: ResultDatabase
  def set(key: String, v: Any): Unit
  def get(key: String): Option[Any]
  def setResult(key: String, v: Any): Unit
  def getResult(key: String): Option[Any]
  def write(key: String, v: Any): Unit
  def writeString(key: String, v: String): Unit
  def read(key: String): Object
  def saveSnapshot(fnameSuffix: String): Unit
}

class CollectorFile(opt: Options) extends Collector {
  // Prepare result database and fill it with the technical parameters 
  override val rdb =
    new ResultDatabaseFile(opt.paramString("outDir", "./"), opt.paramString("outFile", ""))

  opt.allOptions.foreach(t => rdb.put(t._1, t._2))
  opt.retrievedOptions.foreach(t => rdb.put(t._1, t._2))

  println("Result file: " + rdb.fname)
  rdb.put("thisFileName", rdb.fname)

  rdb.setResult("system.hostname", try {
    InetAddress.getLocalHost().getHostName()
  } catch { case e: UnknownHostException => "could-not-determine" })
  rdb.setResult("system.java-version", System.getProperty("java.version"))
  rdb.setResult("system.pid", ManagementFactory.getRuntimeMXBean().getName())
  rdb.put("mainClass", getClass.getName)
  rdb.put("status", "initialized")
  rdb.save()

  override def set(key: String, v: Any) = rdb.put(key, v)
  override def get(key: String) = rdb.get(key)
  override def setResult(key: String, v: Any) = rdb.setResult(key, v)
  override def getResult(key: String) = rdb.getResult(key)
  override def write(key: String, v: Any) = rdb.write(key, v)
  override def writeString(key: String, v: String) = rdb.writeString(key, v)
  override def read(key: String) = rdb.read(key)
  def saveSnapshot(fnameSuffix: String): Unit = {
    opt.retrievedOptions.foreach(t => rdb.put(t._1, t._2)) // in case something changed during run
    rdb.saveSnapshot(fnameSuffix)
  }

  override def close = {
    // Do this again, something may have changed during run:
    opt.retrievedOptions.foreach(t => rdb.put(t._1, t._2))
    rdb.save()
    super.close
  }
  def toSimpleJSON = rdb.map(kv => kv match {
    case (k, v: String)       => s"""\"$k\":\"$v\""""
    case (k, v: Symbol)       => s"""\"$k\":\"${v.toString.tail}\""""
    case (k, v: java.io.File) => s"""\"$k\":\"$v\""""
    case (k, v: Any)          => s"""\"${k}\":${v}"""
  }).mkString("{", ",", "}")
}

object CollectorFile {
  def apply(opt: Options) = new CollectorFile(opt)
}

class CollectorStdout(opt: Options) extends Collector {
  // Prepare result database and fill it with the technical parameters
  val resultPrefix = "result."
  override val rdb = new ResultDatabasePlain()

  opt.allOptions.foreach(t => rdb.put(t._1, t._2))
  opt.retrievedOptions.foreach(t => rdb.put(t._1, t._2))

  rdb.put("system.hostname", try {
    InetAddress.getLocalHost().getHostName()
  } catch { case e: UnknownHostException => "could-not-determine" })
  rdb.put("system.java-version", System.getProperty("java.version"))
  rdb.put("system.pid", ManagementFactory.getRuntimeMXBean().getName())
  rdb.put("mainClass", getClass.getName)
  rdb.put("status", "initialized")

  override def set(key: String, v: Any) = rdb.put(key, v)
  override def get(key: String) = rdb.get(key)
  override def setResult(key: String, v: Any) = rdb.setResult(key, v)
  override def getResult(key: String) = rdb.getResult(key)
  override def write(key: String, v: Any) = rdb.write(key, v)
  override def writeString(key: String, v: String) = rdb.writeString(key, v)
  override def read(key: String) = rdb.read(key)
  def saveSnapshot(fnameSuffix: String): Unit = rdb.saveSnapshot(fnameSuffix)

  override def close = {
    // Do this again, something may have changed during run:
    opt.retrievedOptions.foreach(t => rdb.put(t._1, t._2))
    //rdb.save()
    super.close
  }
}

object CollectorStdout {
  def apply(opt: Options) = new CollectorStdout(opt)
}