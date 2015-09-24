package scevo.mixin

import scala.annotation.tailrec

import scevo.Distribution
import scevo.Preamble.RndApply
import scevo.evo.BinaryTestOutcomes
import scevo.evo.Evaluation
import scevo.evo.ScalarEvaluation
import scevo.evo.ScalarEvaluationMax
import scevo.tools.Options
import scevo.tools.Randomness
import scevo.tools.RngWrapper
import scevo.tools.TRandom
import scevo.evo.BestSelector

/* A selector is intended to operate in two phases: 
 * 1. Creation (based on the previous population). 
 *    When created, a selector can prepare helper data structures 
 *    (or perform 'batch selection', as NSGAII does)
 * 2. Usage. A single applications of next() should return selected individuals. 
 *    next() should never fail, because an algorithm may need to call it more 
 *    than numSelected times
 */

trait Selector[S, E <: Evaluation[_]] {
  def next: EvaluatedSolution[S, E]
  def numSelected: Int
}

trait SelectionHistory[S, E <: Evaluation[_]] {
  def selector(history: Seq[PopulationState[S, E]]): Selector[S, E]
}

trait SelectionLastState[S, E <: Evaluation[_]]
  extends SelectionHistory[S, E] {
  def selector(state: PopulationState[S, E]): Selector[S, E]
  def selector(history: Seq[PopulationState[S, E]]): Selector[S, E] =
    selector(history.head)
}

trait Selection[S, E <: Evaluation[_]]
  extends SelectionLastState[S, E] {
  def selectorSol(solutions: Seq[EvaluatedSolution[S, E]]): Selector[S, E]
  def selector(state: PopulationState[S, E]): Selector[S, E] =
    selectorSol(state.solutions)
}

trait TournamentSel[S, E <: Evaluation[E]]
  extends Selection[S, E] {
  this: Randomness =>
  def tournamentSize: Int
  override def selectorSol(solutions: Seq[EvaluatedSolution[S, E]]) = new Selector[S, E] {
    override val numSelected = solutions.size
    override def next = BestSelectorES(solutions(rng, tournamentSize))
  }
}
trait TournamentSelection[S, E <: Evaluation[E]]
  extends TournamentSel[S, E] {
  this: Options with Randomness =>
  override val tournamentSize = paramInt("tournamentSize", 7, _ >= 2)
}
object TournamentSelection {
  def apply[S, E <: Evaluation[E]](rng: TRandom, tournamentSize_ : Int) =
    new RngWrapper(rng) with TournamentSel[S, E] {
      override def tournamentSize = tournamentSize_
    }
}

trait FitnessProportionateSelection[S, E <: ScalarEvaluationMax]
  extends Selection[S, E] {
  this: Randomness =>
  override def selectorSol(solutions: Seq[EvaluatedSolution[S, E]]) = new Selector[S, E] {
    val distribution = Distribution.fromAnything(solutions.map(_.eval.v))
    override val numSelected = solutions.size
    override def next = solutions(distribution(rng))
  }
}

trait LexicaseSelection[S, E <: BinaryTestOutcomes]
  extends Selection[S, E] {
  this: Randomness =>
  override def selectorSol(solutions: Seq[EvaluatedSolution[S, E]]) = new Selector[S, E] {
    require(solutions.nonEmpty)
    override val numSelected = solutions.size
    override def next = {
      @tailrec def sel(sols: Seq[EvaluatedSolution[S, E]], tests: List[Int]): EvaluatedSolution[S, E] =
        if (sols.size == 1) sols(0)
        else if (tests.size == 1) sols(rng)
        else {
          val theTest = tests(rng)
          val bestEval = BestSelector.select(sols.map( _.eval(theTest)))
          sel(sols.filter(s => s.eval(theTest) == bestEval), tests.diff(List(theTest)))
        }
      sel(solutions, 0.until(solutions(0).eval.size).toList)
    }
  }
}

trait MuLambdaSelection[S, E <: Evaluation[E]]
  extends Selection[S, E] {
  override def selector(history: Seq[PopulationState[S, E]]) = new Selector[S, E] {
    val pool = history.head.solutions ++ (if (history.size > 1)
      history.tail.head.solutions else None)
    private val selected = pool.sortWith(
      (a: EvaluatedSolution[S, E], b: EvaluatedSolution[S, E]) => a.eval.betterThan(b.eval))
    override val numSelected = history.head.solutions.size
    private var i = -1
    override def next = {
      i = (i + 1) % numSelected
      selected(i)
    }
  }
}

trait GreedyBestSelection[S, E <: Evaluation[E]]
  extends Selection[S, E] {
  override def selectorSol(solutions: Seq[EvaluatedSolution[S, E]]) = new Selector[S, E] {
    override val numSelected = 1
    override val next = BestSelectorES(solutions)
  }
}

object BestSelectorES {

  def apply[S, E <: Evaluation[E]](set: Seq[EvaluatedSolution[S, E]]) = 
    set.reduceLeft((a, b) => if (a.eval.betterThan(b.eval)) a else b)

}