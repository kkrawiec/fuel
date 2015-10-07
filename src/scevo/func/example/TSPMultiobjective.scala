package scevo.func.example

import scevo.func.Experiment
import scevo.tools.Rng
import scevo.tools.OptAndColl
import scevo.func.SimpleEA
import scevo.domain.PermutationDomain
import scevo.func.NSGA2Selection
import scevo.tools.Collector
import scevo.tools.Options
import scevo.tools.TRandom
import scevo.domain.Moves
import scevo.domain.Domain
import scevo.func.EA
import scevo.func.RandomMultiOperator
import scevo.func.SimpleBreeder
import scevo.func.NSGA2Selection
import scevo.func.TournamentSelection
import scevo.func.Rank
import scevo.func.StatePop
import scevo.func.StatePop
import scevo.func.Population
import scevo.func.Breeder
import scevo.func.StatePop
import scevo.func.NSGABreeder
import scevo.evo.Dominance

/**
  * Two-objective Traveling Salesperson problem: distance and time.
  *
  * Both objectives minimized.
  */

object TSPMultiobjective {
  def main(args: Array[String]) {
    implicit val (opt, coll) = OptAndColl("--numCities 20")
    implicit val rng = Rng(opt)

    val numCities = opt.paramInt("numCities", _ > 0)
    val size = 10.0
    val cities = 0.until(numCities).map(i => (size * rng.nextDouble, size * rng.nextDouble))
    def dist(i: Int, j: Int) = math.sqrt(
      math.pow(cities(i)._1 - cities(j)._1, 2) + math.pow(cities(i)._2 - cities(j)._2, 2))
    val distances =
      for (i <- 0 until numCities) yield for (j <- 0 until numCities) yield dist(i, j)
    val times =
      for (i <- 0 until numCities) yield for (j <- 0 until numCities) yield (0.5 + rng.nextDouble) * dist(i, j)

    def eval(s: Seq[Int]) = Seq(
      Range(0, s.size).map(i => distances(s(i))(s((i + 1) % s.size))).sum,
      Range(0, s.size).map(i => times(s(i))(s((i + 1) % s.size))).sum)

    implicit val ordering = Dominance[Double] // or Dominance(Ordering[Double])
    val domain = PermutationDomain(numCities)

    Experiment.run(new EA(domain, eval){
      override val breed = new NSGABreeder(domain)
    })
  }
}
/*
    Experiment.run(new EA[Seq[Int], Seq[Double]](domain, eval){
      override val breed = new NSGABreeder[Seq[Int], Double](domain)
    })
    * 
    */
 

 