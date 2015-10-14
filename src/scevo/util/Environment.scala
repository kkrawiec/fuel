package scevo.util

trait Environment{
  implicit def opt: Options 
  implicit def coll: Collector
  implicit def rng : TRandom
}
class Env(args: Array[String] = Array()) extends Environment {
    override implicit val (opt, coll, rng) = OptCollRng(args)
    def apply[R](block : => R) = block
}


