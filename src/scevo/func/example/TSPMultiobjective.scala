package scevo.func.example

import scala.Range
import scevo.moves.PermutationMoves
import scevo.evo.Dominance
import scevo.func.Experiment
import scevo.func.NSGABreeder
import scevo.func.NSGABreederElitist
import scevo.tools.OptCollRng
import scevo.func.EA
import scevo.evo.StatePop
import scevo.func.EACore

/**
  * Two-objective Traveling Salesperson problem: distance and time.
  *
  * Both objectives minimized.
  *
  * See the single objective TSP example in TSP.scala for reference.
  */

object TSPMultiobjective {
  def main(args: Array[String]) {
    implicit val (opt, coll, rng) = OptCollRng("--numCities 12 --populationSize 200")

    // Generate random distance matrix
    val numCities = opt.paramInt("numCities", _ > 0)
    val cities = Seq.fill(numCities)((rng.nextDouble, rng.nextDouble))
    val distances = for (i <- cities) yield for (j <- cities)
      yield math.hypot(i._1 - j._1, i._2 - j._2)
    // Generate random times
    val times = distances.map(_.map(_ * (0.5 + rng.nextDouble)))

    def eval(s: Seq[Int]) = Seq(
      Range(0, s.size).map(i => distances(s(i))(s((i + 1) % s.size))).sum,
      Range(0, s.size).map(i => times(s(i))(s((i + 1) % s.size))).sum)

    implicit val ordering = Dominance[Double] // or Dominance(Ordering[Double])
    val moves = PermutationMoves(numCities)

    // We are using EACore because there is only partial order of the candidate solutions, 
    // so it does not make much sense to report online progress.
    // Non-elitist version
    Experiment.run(new EACore(moves, eval) {
      override val breed = new NSGABreeder(moves)
    })

    // Elitist version (parents and offspring merged in mu+lambda style), 
    // plus simple reporting. 
    Experiment.run(new EACore(moves, eval) {
      override val breed = new NSGABreederElitist(moves)
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
 

 