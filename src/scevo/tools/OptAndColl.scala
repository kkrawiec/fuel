package scevo.tools

import scevo.tools.CollectorFile
import scevo.tools.OptionsFromArgs
import scevo.tools.CollectorFile

object OptAndColl {
  def apply(args: Array[String]) = {
    val opt = new OptionsFromArgs(args)
    (opt, new CollectorFile(opt))
  }
  def apply(args: String) = {
    val opt = new OptionsFromArgs(args)
    (opt, new CollectorFile(opt))
  }
}
/*
object EnvAndRng {
  def apply(args: Array[String]) = {
    val env = new OptionsFromArgs(args) with CollectorFile with Environment
    (env, Rng(env))
  }
  def apply(args: String): (OptionsFromArgs with CollectorFile with Environment, TRandom) = apply(args.split("\\s+"))
}
object EnvWithRng {
  def apply(args: Array[String]) = {
    val env = new EnvFromArgs(args) {
      val rng = Rng(this)
    }
  }
}
* 
*/
