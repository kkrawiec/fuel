package scevo.tools

import scala.collection.immutable.TreeMap


/*
 * Generic option/parameter provider. 
 * TODO: Consider renaming to Parameters
 */
trait Options {
  def options: Map[String, String] // TODO: String => String
  def paramInt(id: String) = options(id).toInt
  def paramInt(id: String, default: Int) = options.getOrElse(id, default.toString).toInt
  def paramDouble(id: String) = options(id).toDouble
  def paramDouble(id: String, default: Int) = options.getOrElse(id, default.toString).toDouble

  def paramInt(id: String, validator: Int => Boolean): Int = {
    val v = paramInt(id)
    assert(validator(v), s"Parameter $id invalidates $validator")
    v
  }
  def paramInt(id: String, default: Int, validator: Int => Boolean): Int = {
    val v = paramInt(id, default)
    assert(validator(v), s"Parameter $id invalidates $validator")
    v
  }
}

abstract class OptionsFromArgs(args: Array[String]) extends Options {
  override lazy val options = OptionParser(args.toList)
}


/*
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
