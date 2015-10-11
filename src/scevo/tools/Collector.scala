package scevo.tools

import java.lang.management.ManagementFactory
import java.net.InetAddress
import java.net.UnknownHostException

/** Collector of results. Each entry is a pair (key,value). 
 *  
 * setResult() should be used for the actual results for experiment, i.e., the
 * 'essential' reporting. set() is more for technical reporting (e.g., timing, 
 * location, files, etc.) 
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

class CollectorFile(opt: Options) extends Collector {
  // Prepare result database and fill it with the technical parameters 
  override val rdb = new ResultDatabase(opt.paramString("outputDir", "./"))
  println("Result file: " + rdb.fname)
  rdb.put("thisFileName", rdb.fname)

  opt.allOptions.foreach(t => rdb.put(t._1, t._2))
  opt.retrievedOptions.foreach(t => rdb.put(t._1, t._2))

  rdb.setResult("system.hostname", try {
    InetAddress.getLocalHost().getHostName()
  } catch { case e: UnknownHostException => "could-not-determine" })
  rdb.setResult("system.java-version", System.getProperty("java.version"));
  rdb.setResult("system.pid", ManagementFactory.getRuntimeMXBean().getName());
  rdb.put("mainClass", getClass.getName)
  rdb.put("status", "initialized")
  rdb.save()

  override def set(key: String, v: Any) = rdb.put(key, v)
  override def setResult(key: String, v: Any) = rdb.setResult(key, v)
  override def write(key: String, v: Any) = rdb.write(key, v)
  override def writeString(key: String, v: String) = rdb.writeString(key, v)
  override def read(key: String) = rdb.read(key)
  def saveSnapshot(fnameSuffix: String): Unit = rdb.saveSnapshot(fnameSuffix)

  override def close = {
    // Do this again, something may have changed during run:
    opt.retrievedOptions.foreach(t => rdb.put(t._1, t._2))
    rdb.save()
    super.close
  }
}