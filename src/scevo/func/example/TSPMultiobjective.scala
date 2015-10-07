package scevo.func.example

import scala.Range
import scevo.domain.PermutationDomain
import scevo.evo.Dominance
import scevo.func.Experiment
import scevo.func.NSGABreederElitist
import scevo.func.StatePop
import scevo.tools.OptAndColl
import scevo.tools.Rng
import scevo.func.EA
import scevo.func.NSGABreeder

/**
  * Two-objective Traveling Salesperson problem: distance and time.
  *
  * Both objectives minimized.
  * 
  * See the single objective TSP example in TSP.scala for reference. 
  */

object TSPMultiobjective {
  def main(args: Array[String]) {
    implicit val (opt, coll) = OptAndColl("--numCities 12 --populationSize 200")
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

    // Non-elitist version
    Experiment.run(new EA(domain, eval) {
      override val breed = new NSGABreeder(domain)
    })

    // Elitist version (parents and offspring merged in mu+lambda style), 
    // plus simple reporting. 
    Experiment.run(new EA(domain, eval) {
      override val breed = new NSGABreederElitist(domain)
      override def epilogue = super.epilogue andThen showParetoFront
      def showParetoFront(s: StatePop[(Seq[Int], Seq[Double])]) = {
        val ranking = breed.nsga.paretoRanking(s.solutions)
        println("Pareto front:")
        println(ranking(0).map(_._2.eval).sortBy(_(0)).mkString("\n"))
        s
      }
    })
  }
}
 

 