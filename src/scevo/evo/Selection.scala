package scevo.evo

import scevo.Distribution
import scevo.Preamble.RndApply
import scevo.tools.Options
import scevo.tools.Randomness

/* Selector is intended to operate in two phases: 
 * 1. When created, it can prepare helper data structures (or perform 'batch selection', as NSGAII does)
 * 2. Then, single applications of next() should return selected individuals. 
 * next() should never fail, because an algorithm may need to call it more than numSelected times
 */

trait Selector[ES <: EvaluatedSolution[_]] {
  def next: ES
  def numSelected: Int
}

trait Selection[ES <: EvaluatedSolution[_]] {
  def selector(history: Seq[PopulationState[ES]]): Selector[ES]
}

trait TournamentSelection[ES <: EvaluatedSolution[_ <: Evaluation]]
  extends Selection[ES] {
  this: Options with Randomness =>
  val tournamentSize = paramInt("tournamentSize", 7, _ >= 2)
  override def selector(history: Seq[PopulationState[ES]]) = new Selector[ES] {
    protected val pool = history.head.solutions
    override val numSelected = pool.size
    override def next = BestSelector(pool(rng, tournamentSize))
  }
}

trait FitnessProportionateSelection[ES <: EvaluatedSolution[_ <: ScalarEvaluation]]
  extends Selection[ES] {
  this: Randomness =>
  override def selector(history: Seq[PopulationState[ES]]) = new Selector[ES] {
    protected val pool = history.head.solutions
    val distribution = Distribution.fromAnything(pool.map(_.eval.v))
    override val numSelected = pool.size
    override def next = pool(distribution(rng))
  }
}

trait MuLambdaSelection[ES <: EvaluatedSolution[_ <: Evaluation]]
  extends Selection[ES] {
  override def selector(history: Seq[PopulationState[ES]]) = new Selector[ES] {
    val pool = history.head.solutions ++ (if (history.size > 1)
      history.tail.head.solutions else None)
    private val selected = pool.sortWith((a, b) => a.eval.betterThan(b.eval))
    override val numSelected = history.head.solutions.size
    private var i = -1
    override def next = {
      i = (i + 1) % numSelected
      selected(i)
    }
  }
}

trait GreedyBestSelection[ES <: EvaluatedSolution[_ <: Evaluation]]
  extends Selection[ES] {
  override def selector(history: Seq[PopulationState[ES]]) = new Selector[ES] {
    override val numSelected = 1
    override val next = BestSelector(history.head.solutions)
  }
}

object BestSelector {
  def apply[ES <: EvaluatedSolution[_ <: Evaluation]](set: Seq[ES]) = {
    require(set.nonEmpty)
    var best = set.head
    set.tail.foreach(e => if (e.eval.betterThan(best.eval)) best = e)
    best
  }
}
