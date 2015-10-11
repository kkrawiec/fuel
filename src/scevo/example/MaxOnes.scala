package scevo.example

import scala.collection.immutable.BitSet

import scevo.func.RunExperiment
import scevo.func.SimpleEA
import scevo.moves.BitSetMoves
import scevo.tools.Env

/**
  * Use case: MaxOnes with GA.
  *
  * Actually implemented as MinOnes (i.e., all bits should be zeroed).
  *
  * This demo shows the probably most compact way of running experiments in ScEvo:
  * first create an environment (which holds the Options, Collector and TRandom),
  * then in the context of that environment create an algorithm, wrap it in Experiment,
  * and run it.
  *
  * Try it in Scala REPL; assuming binaries in directory 'bin', this should work:
  * $ scala -cp bin
  * scala> scevo.example.MaxOnes1
  * scala> scevo.example.MaxOnes2.main()
  * scala> scevo.example.MaxOnes2.main(Array("--numVars", "50"))
  * scala> scevo.example.MaxOnes2.main(Array("--numVars 50"))
  *
  */
object MaxOnes1 {
  new Env {
    RunExperiment(SimpleEA(moves = BitSetMoves(100),
      eval = (s: BitSet) => s.size,
      stop = (s: BitSet, e: Int) => e == 0))
  }
}

object MaxOnes2 {
  def main(args: Array[String]) {
    new Env(Array("--numVars", "500", "--maxGenerations", "200") ++ args) {
      RunExperiment(SimpleEA(
        moves = BitSetMoves(opt.paramInt("numVars", _ > 0)),
        eval = (s: BitSet) => s.size,
        stop = (s: BitSet, e: Int) => e == 0))
    }
  }
}
