package scevo.func

import scevo.tools.Collector
import scevo.tools.Options
import scevo.tools.TRandom
import scevo.tools.CollectorFile
import scevo.tools.OptionsFromArgs
import scevo.tools.Rng

// Does this make sense? To reduce the number of parameters?
trait Environment extends Options with Collector

object EnvFromArgs {
  def apply(args: Array[String]) =
    new OptionsFromArgs(args) with CollectorFile with Environment
}
object EnvAndRng {
  def apply(args: Array[String]) = {
    val env = new OptionsFromArgs(args) with CollectorFile with Environment
    (env, Rng(env))
  }
  def apply(args: String): (OptionsFromArgs with CollectorFile with Environment, TRandom) = apply(args.split("\\s+"))
}
/*
object EnvWithRng {
  def apply(args: Array[String]) = {
    val env = new EnvFromArgs(args) {
      val rng = Rng(this)
    }
  }
}
* 
*/
