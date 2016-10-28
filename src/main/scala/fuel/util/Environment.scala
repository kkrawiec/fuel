package fuel.util

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
  def this(args: (Symbol, Any)*) = this(Options(Map(args: _*)))
  def this(args: Map[Symbol, Any]) = this(Options(args))
  def this(args: Array[String]) = this(Options(args))
  def this(args: String = "") = this(Options(args))
}

trait CollectorProvider {
  implicit def coll: Collector
}
class Coll(implicit opt: Options) extends CollectorProvider {
  override implicit val coll = new CollectorFile(opt)
}

class CollRng(implicit opt: Options) extends Coll with RngProvider {
  override implicit lazy val rng = Rng(opt)
}

class OptColl(options: Options) extends Opt(options) with CollectorProvider {
  override implicit val coll = new CollectorFile(opt)
  def this(args: (Symbol, Any)*) = this(Options(Map(args: _*)))
  def this(args: Map[Symbol, Any]) = this(Options(args))
  def this(args: Array[String]) = this(Options(args))
  def this(args: String = "") = this(Options(args))
}

trait FApp extends App with Environment with CollectorProvider with RngProvider {
  // Need to be lazy because of the order in which args are initialized
  override implicit lazy val opt = Options(args)
  override implicit lazy val rng = Rng(opt)
  override implicit lazy val coll = new CollectorFile(opt)
}

// Instrumented app ?
class IApp(options: Options) extends OptColl(options) with App {
  def this(args: (Symbol, Any)*) = this(Options(Map(args: _*)))
  def this(args: Map[Symbol, Any]) = this(Options(args))
  def this(args: Array[String]) = this(Options(args))
  def this(args: String = "") = this(Options(args))
}
