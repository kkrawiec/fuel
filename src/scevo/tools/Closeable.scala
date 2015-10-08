package scevo.tools

/* To be implemented by the components that need to be closed before program termination
 * 
 */
trait Closeable {
  def close = {}
}

