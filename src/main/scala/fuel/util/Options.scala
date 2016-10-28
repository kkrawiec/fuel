package fuel.util

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

  def getOption(id: String): Option[String] = {
    val v = options(id)
    if (v.isDefined)
      retrievedOptions.put(id, v.get)
    v
  }
  def getOption(id: String, default: Any): String = {
    val v = options(id).getOrElse(default.toString)
    retrievedOptions.put(id, v)
    v
  }
  
  // Below we define getters for most common types, with and without default values, 
  // retrieved by Strings or Symbols, etc. 
  // When adding new getters, do not call options() directly; 
  // rather than that, use getOption(), as it keeps track of the retrieved options. 

  def paramString(id: String) = getOption(id).get
  def paramString(id: String, default: String) = getOption(id, default)
  def apply(id: Symbol, default: String) = getOption(id.name, default)

  // Int
  def paramInt(id: String): Int = getOption(id).getOrElse({ throw new Exception(s"Parameter $id not found"); "" }).toInt
  def paramInt(id: Symbol): Int = paramInt(id.name)
  def apply(id: String) = paramInt(id)

  def paramInt(id: String, default: Int) = getOption(id, default).toInt
  def apply(id: String, default: Int) = paramInt(id, default)
  def apply(id: Symbol, default: Int) = paramInt(id.name, default)

  def paramInt(id: String, validator: Int => Boolean): Int = {
    val v = paramInt(id)
    assert(validator(v), s"Parameter $id invalidates validator")
    v
  }
  def paramInt(id: Symbol, validator: Int => Boolean): Int = paramInt(id.name, validator)
  def apply(id: String, validator: Int => Boolean) = paramInt(id, validator)
  def apply(id: Symbol, validator: Int => Boolean) = paramInt(id.name, validator)

  def paramInt(id: String, default: Int, validator: Int => Boolean): Int = {
    val v = paramInt(id, default)
    assert(validator(v), s"Parameter $id invalidates validator")
    v
  }
  def apply(id: String, default: Int, validator: Int => Boolean): Int =
    paramInt(id, default, validator)
  def apply(id: Symbol, default: Int, validator: Int => Boolean): Int =
    paramInt(id.name, default, validator)

  // Double
  def paramDouble(id: String) = getOption(id).get.toDouble
  def paramDouble(id: Symbol) = getOption(id.name).get.toDouble
  def paramDouble(id: String, default: Double) = getOption(id, default).toDouble
  def paramDouble(id: Symbol, default: Double) = getOption(id.name, default).toDouble
  def apply(id: String, default: Double) = getOption(id, default).toDouble
  def apply(id: Symbol, default: Double) = getOption(id.name, default).toDouble

  def paramDouble(id: String, validator: Double => Boolean): Double = {
    val v = paramDouble(id)
    assert(validator(v), s"Parameter $id invalidates validator")
    v
  }
  def paramDouble(id: Symbol, validator: Double => Boolean): Double = paramDouble(id.name, validator)
  def apply(id: String, validator: Double => Boolean): Double = paramDouble(id, validator)
  def apply(id: Symbol, validator: Double => Boolean): Double = paramDouble(id.name, validator)

  def paramDouble(id: String, default: Double, validator: Double => Boolean): Double = {
    val v = paramDouble(id, default)
    assert(validator(v), s"Parameter $id invalidates validator")
    v
  }
  def apply(id: String, default: Double, validator: Double => Boolean): Double =
    paramDouble(id, default, validator)
  def apply(id: Symbol, default: Double, validator: Double => Boolean): Double =
    paramDouble(id.name, default, validator)

  // Boolean
  def paramBoolReq(id: String): Boolean = getOption(id) match {
    case Some("true")  => true
    case Some("false") => false
    case _             => throw new Exception(s"Boolean value expected for parameter $id")
  }
  def paramBoolReq(id: Symbol): Boolean = paramBoolReq(id.name)
  def paramBool(id: String, default: Boolean = false): Boolean = getOption(id, default) match {
    case "true"  => true
    case "false" => false
    case _       => throw new Exception(s"Boolean value expected for parameter $id")
  }
  def paramBool(id: Symbol): Boolean = paramBool(id.name)
  def apply(id: String, default: Boolean) = paramBool(id, default)
  def apply(id: Symbol, default: Boolean) = paramBool(id.name, default)

  // Enumeration
  def apply(id: Enumeration) = {
    val enumName = id.getClass.getSimpleName.replace("$", "")
    id.withName(paramString(enumName))
  }

  def ++(other: Options): Options
  def ++(other: Map[Symbol, Any]): Options
  def +(entry: (Symbol, Any)): Options
}

class OptionsMap(opt: Map[String, String]) extends Options {
  override def allOptions = opt
  override protected def options = (id: String) => opt.get(id)
  override def ++(other: Options): Options = new OptionsMap(allOptions ++ other.allOptions)
  def ++(other: Map[Symbol, Any]): Options = this ++ Options(other)
  def +(entry: (Symbol, Any)): Options = this ++ Options(Map(entry))
}

object Options {
  def apply(parentOpt: Options, m: (Symbol, Any)*) = new OptionsMap(
    parentOpt.allOptions ++ m.map(e => (e._1.name, e._2.toString)).toMap)
  def apply(m: (Symbol, Any)*) = new OptionsMap(m.map(e => (e._1.name, e._2.toString)).toMap)
  def apply(m: Map[Symbol, Any]) = new OptionsMap(m.map(e => (e._1.name, e._2.toString)))
  def apply(args: Array[String]) = new OptionsMap(OptionParser(args))
  def apply(params: String): OptionsMap = apply(
    if (params.trim == "") Array[String]() else params.trim.split("\\s+"))
}

trait NoOptions extends Options {
  override def allOptions = Map[String, String]()
  override protected def options = (_: String) => None
  override def ++(other: Options) = this
  def ++(other: Map[Symbol, Any]) = this
  def +(entry: (Symbol, Any)) = this
}
object NoOptions {
  def apply = new NoOptions {}
}

object OptionParser {
  // Convenience hack: if only one arg, then check if it's single string with params.
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
