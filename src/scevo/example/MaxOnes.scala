package scevo.example

import scala.collection.immutable.BitSet
import scevo.func.RunExperiment
import scevo.func.SimpleEA
import scevo.moves.BitSetMoves
import scevo.util.Env
import scevo.util.EnvArgs

/**
  * Use case: MaxOnes with genetic algorithm (GA), using default parameter settings.
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
  * Or:
  * $ scala -cp ./bin -e scevo.example.MaxOnes1
  *
  */
object MaxOnes1 {
  new EnvArgs {
    RunExperiment(SimpleEA(moves = BitSetMoves(100),
      eval = (s: BitSet) => s.size,
      stop = (s: BitSet, e: Int) => e == 0))
  }
}

/**
  * A variant with some parameters set using Options object (opt).
  *
  */
object MaxOnes2 {
  def main(args: Array[String]) {
    new Env(Map('numVars -> 500, 'maxGenerations -> 200)) {
      RunExperiment(SimpleEA(
        moves = BitSetMoves(opt('numVars, (_:Int) > 0)),
        eval = (s: BitSet) => s.size,
        stop = (s: BitSet, e: Int) => e == 0))
    }
  }
}
