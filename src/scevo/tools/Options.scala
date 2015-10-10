package scevo.tools

import scala.collection.immutable.TreeMap

/*
 * Generic option/parameter provider. 
 * TODO: Consider renaming to Parameters
 */
trait Options {
  protected def options: String => Option[String]

  def allOptions: Map[String, String]
  override def toString = allOptions.toString

  // Stores the values of retrieved options, *including the default values*
  lazy val retrievedOptions = scala.collection.mutable.Map[String, String]()

  def warnNonRetrieved = {
    val nonRetrieved = allOptions.toList.diff(retrievedOptions.toList)
    if (nonRetrieved.nonEmpty)
      println("WARNING: The following options have been set but not retrievied:\n" +
        nonRetrieved.mkString("\n"))
  }

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

  // TODO: not consequent: paramString returns Option, while paramInt a value
  def paramString(id: String) = getOption(id)
  def paramString(id: String, default: String) = getOption(id, default)

  def paramInt(id: String) = getOption(id).getOrElse({ throw new Exception(s"Parameter $id not found"); "" }).toInt
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
  def paramBoolReq(id: String) = getOption(id) match {
    case Some("true")  => true
    case Some("false") => false
    case _             => throw new Exception(s"Boolean value expected for parameter $id")
  }
  def paramBool(id: String, default: Boolean = false) = getOption(id, default) match {
    case "true"  => true
    case "false" => false
    case _       => throw new Exception(s"Boolean value expected for parameter $id")
  }
}

abstract class OptionsC(opt: Map[String, String]) extends Options {
  override def allOptions = opt
  override protected def options = (id: String) => opt.get(id)
}

class OptionsFromArgs(args: Array[String])
    extends OptionsC(OptionParser(args)) {
  def this(params: String) =
    this(if (params.trim == "") Array[String]() else params.trim.split("\\s+"))
}

class NoOptions extends Options {
  override def allOptions = Map[String, String]()
  override protected def options = (_: String) => None
}
object NoOptions {
  def apply = new NoOptions
}

/*
 * TODO: Make the parsing more robust
 */

object OptionParser {
  // Convenience hack: if only one arg, then check if it's a one string with params.
  def apply(arr: Array[String]) = nextOption(
    if (arr.size == 1) arr(0).split("\\s+").toList
    else arr.toList)
  def apply(list: List[String]) = nextOption(list)
  private def nextOption(list: List[String]): Map[String, String] = {
    list match {
      case Nil => TreeMap[String, String]()
      case option :: value :: tail =>
        if (option.substring(0, 2) == "--")
          Map(option.slice(2, option.length) -> value) ++ nextOption(tail)
        else throw new Exception(s"An option should start with '--': $option ")
      case option :: tail => throw new Exception(f"Unknown option: '$option'")
    }
  }
}
