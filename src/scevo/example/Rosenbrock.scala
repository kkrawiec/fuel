package scevo.example

import scala.Range

import scevo.func.RunExperiment
import scevo.func.SimpleEA
import scevo.moves.DoubleVectorMoves
import scevo.util.OptCollRng

/**
  * Continuous optimization: Rosenbrock function.
  *
  * Minimized fitness function.
  */
object Rosenbrock {
  def main(args: Array[String]) {
    implicit val (opt, coll, rng) = OptCollRng("--n 3 --maxGenerations 300 --printResults true")

    val n = opt.paramInt('n, (_:Int) > 0) // dimensionality of the problem/space
    def rosenbrock(x: Seq[Double]) = Range(0, n - 1).map(i =>
      (1 - x(i)) * (1 - x(i)) + 100 * math.pow(x(i + 1) - x(i) * x(i), 2)).sum

    RunExperiment(SimpleEA(new DoubleVectorMoves(n, 0.001), rosenbrock))
  }
}


