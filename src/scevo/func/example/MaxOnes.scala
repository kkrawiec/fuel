package scevo.func.example

import scala.collection.immutable.BitSet

import scevo.func.Experiment
import scevo.func.SimpleEA
import scevo.moves.BitSetMoves
import scevo.tools.OptCollRng

/**
  * Use case: MaxOnes with GA.
  *
  * Actually implemented as MinOnes (i.e., all bits should be zeroed). 
  */
object MaxOnes {
  def main(args: Array[String]) {
    implicit val (opt, coll, rng) = OptCollRng("--numVars 500 --maxGenerations 200")

    val ga = new SimpleEA(
      moves = BitSetMoves(opt.paramInt("numVars", _ > 0)),
      eval = (s: BitSet) => s.size,
      stop = (s: BitSet, e: Int) => e == 0)

    Experiment.run(ga)
  }
}
