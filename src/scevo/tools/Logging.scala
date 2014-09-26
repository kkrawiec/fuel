package scevo.tools

trait Logging {

  var log = List[String]()
  def log(s: String) {
    println(s)
    log = log :+ s
  }
}