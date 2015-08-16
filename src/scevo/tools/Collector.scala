package scevo.tools

import java.util.Calendar
import java.lang.management.ManagementFactory
import java.net.InetAddress
import java.net.UnknownHostException

/* Result collector
 * 
 */

trait Collector extends Closeable {
  def rdb: ResultDatabase
  def set(key: String, v: Any):Unit
  def setResult(key: String, v: Any):Unit
  def write(key: String, v: Any):Unit
  def writeString(key: String, v: String):Unit
  def read(key: String):Object
  def saveSnapshot(fnameSuffix: String): Unit
}

trait CollectorFile extends Collector {
  this: Options =>
  // Prepare result database and fill it with the technical parameters 
  override val rdb = new ResultDatabase(paramString("outputDir", "./"))
  println("Result file: " + rdb.fname)

  allOptions.foreach(t => rdb.put(t._1, t._2))
  retrievedOptions.foreach(t => rdb.put(t._1, t._2))

  rdb.setResult("system.hostname", try {
    InetAddress.getLocalHost().getHostName()
  } catch { case e: UnknownHostException => "could-not-determine" })
  rdb.setResult("system.java-version", System.getProperty("java.version"));
  rdb.setResult("system.pid", ManagementFactory.getRuntimeMXBean().getName());
  rdb.setResult("system.startTime", Calendar.getInstance().getTime().toString)
  rdb.put("mainClass", getClass.getName)
  rdb.put("status", "initialized")
  rdb.saveWorkingState()

  override def set(key: String, v: Any) = rdb.put(key, v)
  override def setResult(key: String, v: Any) = rdb.setResult(key, v)
  override def write(key: String, v: Any) = rdb.write(key, v)
  override def writeString(key: String, v: String) = rdb.writeString(key, v)
  override def read(key: String) = rdb.read(key)
  def saveSnapshot(fnameSuffix: String): Unit = rdb.saveSnapshot(fnameSuffix)

  override def close = {
    // Do this again, something may have changed during run:
    retrievedOptions.foreach(t => rdb.put(t._1, t._2))
    rdb.save
    super.close
  }
}