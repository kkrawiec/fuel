package scevo.util

trait Environment {
  implicit def opt: Options
  implicit def coll: Collector
  implicit def rng: TRandom
}

class EnvArgs(args: Array[String]) extends Environment {
  override implicit val (opt, coll, rng) = OptCollRng(args)
  def this(args: String = "") = this(args.trim.split("\\s+"))
  def apply[R](block: => R) = block
}

class Env(args: Map[Symbol, Any]) extends Environment {
  override implicit val (opt, coll, rng) = OptCollRng(args)
}
 
