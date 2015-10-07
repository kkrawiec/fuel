package scevo.tools

/**
  * Convenience functions for simultaneous construction of most common
  *  environment.
  */

object OptColl {
  def apply(args: Array[String]) = {
    val opt = new OptionsFromArgs(args)
    (opt, new CollectorFile(opt))
  }
  def apply(args: String) = {
    val opt = new OptionsFromArgs(args)
    (opt, new CollectorFile(opt))
  }
}

object OptCollRng {
  def apply(args: Array[String]) = {
    val optColl = OptColl(args)
    (optColl._1, optColl._2, Rng(optColl._1))
  }
  def apply(args: String) = {
    val optColl = OptColl(args)
    (optColl._1, optColl._2, Rng(optColl._1))
  }
}

