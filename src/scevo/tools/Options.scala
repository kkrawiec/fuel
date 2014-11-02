package scevo.tools

import scala.collection.immutable.TreeMap

/*
 * Generic option/parameter provider. 
 * TODO: Consider renaming to Parameters
 */
trait Options {
  protected def options: String => Option[String]

  // Stores the values of retrieved options, *including the default values*
  val retrievedOptions = scala.collection.mutable.Map[String, String]()

  protected def getOption(id: String) = {
    val v = options(id)
    if (v.isDefined)
      retrievedOptions.put(id, v.get)
    v
  }
  protected def getOption(id: String, default: Any) = {
    val v = options(id).getOrElse(default.toString)
    retrievedOptions.put(id, v)
    v
  }

  def paramString(id: String) = getOption(id)
  def paramInt(id: String) = getOption(id).get.toInt
  def paramInt(id: String, default: Int) = getOption(id, default).toInt
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
  def paramDouble(id: String) = options(id).get.toDouble
  def paramDouble(id: String, default: Double) = getOption(id, default).toDouble
  def paramDouble(id: String, validator: Double => Boolean): Double = {
    val v = paramDouble(id)
    assert(validator(v), s"Parameter $id invalidates $validator")
    v
  }
  def paramDouble(id: String, default: Double, validator: Double => Boolean): Double = {
    val v = paramDouble(id, default)
    assert(validator(v), s"Parameter $id invalidates $validator")
    v
  }
}

abstract class OptionsFromArgs(args: Array[String]) extends Options {
  def this(params: String) = this(params.split("\\s+"))
  private val opt = OptionParser(args.toList)
  override protected def options = (id: String) => opt.get(id)
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
