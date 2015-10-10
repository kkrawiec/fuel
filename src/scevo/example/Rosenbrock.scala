package scevo.example

import scala.Range
import scevo.func.Experiment
import scevo.func.SimpleEA
import scevo.moves.DoubleVectorMoves
import scevo.tools.OptCollRng

/**
  * Continuous optimization: Rosenbrock function.
  *
  * Minimized fitness function.
  */
object Rosenbrock {
  def main(args: Array[String]) {
    implicit val (opt, coll, rng) = OptCollRng("--n 3 --maxGenerations 300")

    val n = opt.paramInt("n", _ > 0) // dimensionality of the problem/space
    def rosenbrock(x: Seq[Double]) = Range(0, n - 1).map(i =>
      (1 - x(i)) * (1 - x(i)) + 100 * math.pow(x(i + 1) - x(i) * x(i), 2)).sum

    Experiment.run(SimpleEA(new DoubleVectorMoves(n, 0.001), rosenbrock))
  }
}


