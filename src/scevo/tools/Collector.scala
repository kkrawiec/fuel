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

  override def close = {
    // Do this again, something may have changed during run:
    retrievedOptions.foreach(t => rdb.put(t._1, t._2))
    rdb.save
    super.close
  }
}