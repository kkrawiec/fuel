package scevo.tools

import scala.collection.immutable.TreeMap

/**
 * TODO: Make the parsing more robust
 */

object OptionParser {

  def apply(list: List[String]) = nextOption(list)

  private def nextOption(list: List[String]): Map[String, String] = {
    list match {
      case Nil => TreeMap[String, String]()
      case option :: value :: tail =>
        Map(option.slice(2, option.length) -> value) ++ nextOption(tail)
      // (slicing to remove the --)
      case option :: tail => throw new Exception("Unknown option " + option)
    }
  }
}
