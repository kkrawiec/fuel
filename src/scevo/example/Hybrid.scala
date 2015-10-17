package scevo.example

import scevo.func.RunExperiment
import scevo.func.SimpleEA
import scevo.func.SimpleSteadyStateEA
import scevo.util.GetOptCollRng
import scevo.moves.DoubleVectorMoves
import scevo.func.RemoveEvals
import scevo.util.OptColl

/** A simple example of creating a hybrid algorithm: generational EA followed by steady-state EA. 
 *  
 */
object Hybrid extends App {
  new OptColl('n -> 3, 'maxGenerations -> 300, 'printResults -> true) {

    val n = opt('n, (_: Int) > 0) // dimensionality of the problem/space
    def rosenbrock(x: Seq[Double]) = Range(0, n - 1).map(i =>
      (1 - x(i)) * (1 - x(i)) + 100 * math.pow(x(i + 1) - x(i) * x(i), 2)).sum

    val moves = new DoubleVectorMoves(n, 0.001)

    // Create two algorithms, overriding the number of generations for the second one:
    val ea = SimpleEA(moves, rosenbrock)
    val ssea = SimpleSteadyStateEA(moves, rosenbrock)(opt + ('maxGenerations -> 10000), 
        coll, rng, Ordering[Double])
    
    // Compose them into one function: 
    val hybridAlgorithm = ea.initialize andThen ea andThen ssea

    RunExperiment(hybridAlgorithm)
  }
}

