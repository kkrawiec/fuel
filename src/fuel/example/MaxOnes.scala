package fuel.example

import scala.collection.immutable.BitSet
import fuel.func.RunExperiment
import fuel.func.SimpleEA
import fuel.moves.BitSetMoves
import fuel.util.ScApp
import fuel.util.IApp

/**
  * MaxOnes with genetic algorithm (GA), using default parameter settings.
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
  * scala> fuel.example.MaxOnes1
  * scala> fuel.example.MaxOnes2.main()
  * scala> fuel.example.MaxOnes2.main(Array("--numVars", "50"))
  * scala> fuel.example.MaxOnes2.main(Array("--numVars 50"))
  *
  * Or:
  * $ scala -cp ./bin -e fuel.example.MaxOnes1
  *
  */
object MaxOnes1 extends ScApp {
  RunExperiment(SimpleEA(moves = BitSetMoves(100),
    eval = (s: BitSet) => s.size,
    stop = (s: BitSet, e: Int) => e == 0))
}

/**
  * A variant with some parameters set manually.
  *
  */
object MaxOnes2 extends IApp('numVars -> 500, 'maxGenerations -> 200,
  'printResults -> true) {
  RunExperiment(SimpleEA(
    moves = BitSetMoves(opt('numVars, (_: Int) > 0)),
    eval = (s: BitSet) => s.size,
    optimalValue = 0))
}

/** A super short variant 
 *  
 */
object MaxOnes3 extends ScApp {
  RunExperiment(SimpleEA(BitSetMoves(100), (s: BitSet) => s.size, 0))
}