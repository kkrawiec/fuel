package scevo.func.example

import scala.collection.immutable.BitSet

import scevo.domain.BitSetDomain
import scevo.func.Experiment
import scevo.func.SimpleEA
import scevo.tools.OptAndColl
import scevo.tools.Rng

/**
  * Use case: MaxOnes with GA.
  *
  */
object MaxOnes {
  def main(args: Array[String]) {
    implicit val (opt, coll) = OptAndColl("--numVars 500  --maxGenerations 1000 --populationSize 1000 ")
    implicit val rng = Rng(opt)

    val ga = new SimpleEA[BitSet, Int](
      domain = BitSetDomain(opt.paramInt("numVars", _ > 0)),
      eval = (s: BitSet) => s.size,
      stop = (s: BitSet, e: Int) => e == 0)

    // Create the experiment and launch it:
    val exp = Experiment(ga)
    exp()
  }
}
