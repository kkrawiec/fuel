package scevo.util

trait Environment {
  def opt: Options
}
trait RngProvider {
  def rng: TRandom
}
class Opt(options: Options) extends Environment with RngProvider {
  override implicit val opt = options
  // RNG is not always needed so let's make it lazy: 
  override implicit lazy val rng = Rng(opt)
  def this(args: (Symbol, Any)*) = this(OptionsMap(Map(args: _*)))
  def this(args: Map[Symbol, Any]) = this(OptionsMap(args))
  def this(args: Array[String]) = this(OptionsMap(args))
  def this(args: String = "") = this(OptionsMap(args))
}

trait CollectorProvider {
  implicit def coll: Collector
}
class Coll(implicit val opt: Options) extends CollectorProvider {
  override implicit val coll = new CollectorFile(opt)
}

class OptColl(options: Options) extends Opt(options) with CollectorProvider {
  override implicit val coll = new CollectorFile(opt)
  def this(args: (Symbol, Any)*) = this(OptionsMap(Map(args: _*)))
  def this(args: Map[Symbol, Any]) = this(OptionsMap(args))
  def this(args: Array[String]) = this(OptionsMap(args))
  def this(args: String = "") = this(OptionsMap(args))
}

trait ScApp extends App with Environment with CollectorProvider with RngProvider {
  // Need to be lazy because of the order in which args are initialized
  override implicit lazy val opt = OptionsMap(args)
  override implicit lazy val rng = Rng(opt)
  override implicit lazy val coll = new CollectorFile(opt)
}

// Instrumented app ?
class IApp(options: Options) extends OptColl(options) with App {
  def this(args: (Symbol, Any)*) = this(OptionsMap(Map(args: _*)))
  def this(args: Map[Symbol, Any]) = this(OptionsMap(args))
  def this(args: Array[String]) = this(OptionsMap(args))
  def this(args: String = "") = this(OptionsMap(args))
}
