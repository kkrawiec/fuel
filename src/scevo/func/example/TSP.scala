package scevo.func.example

import scevo.func.Experiment
import scevo.tools.Rng
import scevo.tools.OptAndColl
import scevo.func.SimpleEA
import scevo.domain.PermutationDomain

/**
  *  Traveling Salesperson problem.
  * Minimized fitness function.
  */

object TSP {
  def main(args: Array[String]) {
    implicit val (opt, coll) = OptAndColl("--numCities 8")
    implicit val rng = Rng(opt)

    val numCities = opt.paramInt("numCities", _ > 0)
    val size = 10.0
    val cities = 0.until(numCities).map(i => (size * rng.nextDouble, size * rng.nextDouble))
    def dist(i: Int, j: Int) = math.sqrt(
      math.pow(cities(i)._1 - cities(j)._1, 2) + math.pow(cities(i)._2 - cities(j)._2, 2))
    val distances =
      for (i <- 0 until numCities) yield for (j <- 0 until numCities) yield dist(i, j)

    def eval(s: Seq[Int]) =
      Range(0, s.size).map(i => distances(s(i))(s((i + 1) % s.size))).sum

    Experiment.run(SimpleEA(PermutationDomain(numCities), eval))
  }
}


