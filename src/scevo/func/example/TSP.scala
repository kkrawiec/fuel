package scevo.func.example

import scala.Range
import scevo.domain.PermutationDomain
import scevo.func.Experiment
import scevo.func.SimpleEA
import scevo.tools.OptCollRng

/**
  * Traveling Salesperson problem.
  *
  * Minimized fitness function.
  */

object TSP {
  def main(args: Array[String]) {
    implicit val (opt, coll, rng) = OptCollRng("--numCities 8")

    // Generate random distance matrix
    val numCities = opt.paramInt("numCities", _ > 0)
    val cities = for (_ <- 1 to numCities) yield (rng.nextDouble, rng.nextDouble)
    val distances = for (i <- cities) yield for (j <- cities)
      yield math.hypot(i._1 - j._1, i._2 - j._2)

    // Fitness function
    def eval(s: Seq[Int]) =
      Range(0, s.size).map(i => distances(s(i))(s((i + 1) % s.size))).sum

    Experiment.run(SimpleEA(PermutationDomain(numCities), eval))
  }
}


