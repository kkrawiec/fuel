package scevo.example

import scala.Range
import scevo.core.Dominance
import scevo.core.StatePop
import scevo.func.NSGABreeder
import scevo.func.NSGABreederElitist
import scevo.func.PartialTournament
import scevo.func.RandomMultiOperator
import scevo.func.RunExperiment
import scevo.func.SimpleBreeder
import scevo.moves.PermutationMoves
import scevo.util.Coll
import scevo.util.Opt
import scevo.util.TRandom
import scevo.func.EACore
import scevo.func.ParallelEval

/**
  * Two-objective Traveling Salesperson problem: distance and cost.
  *
  * Both objectives minimized.
  *
  * See the single objective TSP example in TSP.scala for reference.
  */
object TSPMultiobjective extends App {
  new Opt('numCities -> 12, 'populationSize -> 200) {

    val numCities = opt.paramInt('numCities, (_: Int) > 0)

    val moves = PermutationMoves(numCities)
    val problem = new TSPMOProblem(numCities)
    implicit val ordering = Dominance[Double] // or Dominance(Ordering[Double])

    // Using EACore because there is only partial order of solutions, 
    // so it does not make much sense to report online progress.

    // 1. Naive algorithm using partial tournament (dominance tournament) of size 2
    // Running every experiment in a Coll environment creates separate collector files
    new Coll {
      RunExperiment(new EACore(moves, ParallelEval(problem.eval)) {
        def selection = new PartialTournament[Seq[Int], Seq[Double]](2)
        override def iter = SimpleBreeder(selection, moves.moves: _*) andThen
          evaluate
      })
    }

    // 2. NSGAII non-elitist version
    new Coll {
      RunExperiment(new EACore(moves, ParallelEval(problem.eval)) {
        override val iter = new NSGABreeder(moves) andThen evaluate
      })
    }

    // 3. NSGAII elitist version (parents and offspring merged in mu+lambda style), 
    // plus simple reporting. 
    new Coll {
      RunExperiment(new EACore(moves, ParallelEval(problem.eval)) {
        val breeder = new NSGABreederElitist(moves)
        override val iter = breeder andThen evaluate
        override def algorithm = super.algorithm andThen showParetoFront
        def showParetoFront(s: StatePop[(Seq[Int], Seq[Double])]) = {
          val ranking = breeder.nsga.paretoRanking(s)
          println("Pareto front:")
          println(ranking.head.map(_._2.eval).sortBy(_(0)).mkString("\n"))
          s
        }
      })
    }
  }
}

/**
  * Implements a random instance of TSP problem with n cities and two objectives:
  *  route length and cost.
  */
class TSPMOProblem(val n: Int)(implicit rng: TRandom) {
  // Generate random locations of cities on 2D unitary plane
  val cities = Seq.fill(n)((rng.nextDouble, rng.nextDouble))
  val distances = for (i <- cities) yield for (j <- cities)
    yield math.hypot(i._1 - j._1, i._2 - j._2)
  // Generate random costs
  val costs = distances.map(_.map(_ * (0.5 + rng.nextDouble)))

  def eval(s: Seq[Int]) = Seq(
    Range(0, s.size).map(i => distances(s(i))(s((i + 1) % s.size))).sum,
    Range(0, s.size).map(i => costs(s(i))(s((i + 1) % s.size))).sum)
}
 