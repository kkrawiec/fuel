package fuel.example

import scala.Range
import fuel.func.RunExperiment
import fuel.func.SimpleEA
import fuel.func.SimpleSteadyStateEA
import fuel.moves.DoubleVectorMoves
import fuel.util.OptColl

/**
  * Continuous optimization: Rosenbrock function.
  *
  * Minimized fitness function.
  */
object Rosenbrock extends App {
  new OptColl('n -> 3, 'maxGenerations -> 300, 'printResults -> true) {

    val n = opt.paramInt('n, (_:Int) > 0) // dimensionality of the problem/space
    def rosenbrock(x: Seq[Double]) = Range(0, n - 1).map(i =>
      (1 - x(i)) * (1 - x(i)) + 100 * math.pow(x(i + 1) - x(i) * x(i), 2)).sum

    RunExperiment(SimpleEA(new DoubleVectorMoves(n, 0.001), rosenbrock))
  }
}


/** Steady-state variant 
 *  
 */
object Rosenbrock2 extends App {
  new OptColl('n -> 3, 'maxGenerations -> 300000, 'printResults -> true) {

    val n = opt.paramInt('n, (_:Int) > 0) // dimensionality of the problem/space
    def rosenbrock(x: Seq[Double]) = Range(0, n - 1).map(i =>
      (1 - x(i)) * (1 - x(i)) + 100 * math.pow(x(i + 1) - x(i) * x(i), 2)).sum

    RunExperiment(SimpleSteadyStateEA(new DoubleVectorMoves(n, 0.001), rosenbrock))
  }
}
