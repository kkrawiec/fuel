package scevo.evo

import scala.Ordering

import scevo.Distribution
import scevo.Preamble.RndApply
import scevo.tools.Options
import scevo.tools.Randomness
import scevo.tools.RngWrapper
import scevo.tools.TRandom

/* Selector is intended to operate in two phases: 
 * 1. When created, it can prepare helper data structures (or perform 'batch selection', as NSGAII does)
 * 2. Then, single applications of next() should return selected individuals. 
 * next() should never fail, because an algorithm may need to call it more than numSelected times
 */

trait Selector[S <: Solution, E <: Evaluation] {
  def next: EvaluatedSolution[S, E]
  def numSelected: Int
}

trait SelectionHistory[S <: Solution, E <: Evaluation] {
  def selector(history: Seq[PopulationState[S, E]]): Selector[S, E]
}

trait SelectionLastState[S <: Solution, E <: Evaluation]
  extends SelectionHistory[S, E] {
  def selector(state: PopulationState[S, E]): Selector[S, E]
  def selector(history: Seq[PopulationState[S, E]]): Selector[S, E] =
    selector(history.head)
}

trait Selection[S <: Solution, E <: Evaluation]
  extends SelectionLastState[S, E] {
  def selectorSol(solutions: Seq[EvaluatedSolution[S, E]]): Selector[S, E]
  def selector(state: PopulationState[S, E]): Selector[S, E] =
    selectorSol(state.solutions)
}

trait TournamentSel[S <: Solution, E <: Evaluation]
  extends Selection[S, E] {
  this: Randomness =>
  def tournamentSize: Int
  override def selectorSol(solutions: Seq[EvaluatedSolution[S, E]]) = new Selector[S, E] {
    override val numSelected = solutions.size
    override def next = BestSelector(solutions(rng, tournamentSize))
  }
}
trait TournamentSelection[S <: Solution, E <: Evaluation]
  extends TournamentSel[S, E] {
  this: Options with Randomness =>
  override val tournamentSize = paramInt("tournamentSize", 7, _ >= 2)
}
object TournamentSelection {
  def apply[S <: Solution, E <: Evaluation](rng: TRandom, tournamentSize_ : Int) =
    new RngWrapper(rng) with TournamentSel[S, E] {
      override def tournamentSize = tournamentSize_
    }
}

trait FitnessProportionateSelection[S <: Solution, E <: ScalarEvaluationMax]
  extends Selection[S, E] {
  this: Randomness =>
  override def selectorSol(solutions: Seq[EvaluatedSolution[S, E]]) = new Selector[S, E] {
    val distribution = Distribution.fromAnything(solutions.map(_.eval.v))
    override val numSelected = solutions.size
    override def next = solutions(distribution(rng))
  }
}

trait LexicaseSelection[S <: Solution, E <: BinaryTestOutcomes]
  extends Selection[S, E] {
  this: Randomness =>
  override def selectorSol(solutions: Seq[EvaluatedSolution[S, E]]) = new Selector[S, E] {
    override val numSelected = solutions.size
    override def next = {
      def sel(sols: Seq[EvaluatedSolution[S, E]], cases: List[Int]): EvaluatedSolution[S, E] =
        if (sols.size == 1)
          sols(0)
        else if( cases.size == 1)
          sols(rng)
          else {
            val theCase = cases(rng)
            val maxEval = sols.map(_.eval(theCase).v).max
            //println("Sols:" + sols.size + " Cases: " + cases.size)
            sel(sols.filter(s => s.eval(theCase).v == maxEval), cases.diff(List(theCase)))
          }
      sel(solutions, 0.until(numSelected).toList )
    }
  }
}
/* innefective, memory hog
trait LexicaseSelection[S <: Solution, E <: BinaryTestOutcomes]
  extends Selection[S, E] {
  this: Randomness =>
  override def selectorSol(solutions: Seq[EvaluatedSolution[S, E]]) = new Selector[S, E] {
    override val numSelected = solutions.size
    val permutations = 0.until(numSelected).permutations.toStream
    override def next = {
      def sel(sols: Seq[EvaluatedSolution[S, E]], cases: List[Int]): EvaluatedSolution[S, E] =
        if (sols.size == 1)
          sols(0)
        else cases match {
          case Nil => sols(rng)
          case c :: tail => {
            val maxEval = sols.map(_.eval(c).v).max
            sel(sols.filter(s => s.eval(c).v == maxEval), tail)
          }
        }
      sel(solutions, permutations(rng).toList)
    }
  }
}
*/
trait MuLambdaSelection[S <: Solution, E <: ScalarEvaluation]
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

trait GreedyBestSelection[S <: Solution, E <: Evaluation]
  extends Selection[S, E] {
  override def selectorSol(solutions: Seq[EvaluatedSolution[S, E]]) = new Selector[S, E] {
    override val numSelected = 1
    override val next = BestSelector(solutions)
  }
}

object BestSelector {
  def apply[S <: Solution, E <: Evaluation](set: Seq[Tuple2[S, E]]) =
    set.reduceLeft((a, b) => if (a._2.betterThan(b._2)) a else b)

  def apply[S <: Solution, E <: Evaluation](set: Seq[EvaluatedSolution[S, E]]) =
    set.reduceLeft((a, b) => if (a.eval.betterThan(b.eval)) a else b)

  // I'd be happy to call this 'apply' as well, but type erasure does not permit.
  def select[E <: Evaluation](set: Seq[E]) =
    set.reduceLeft((a, b) => if (a.betterThan(b)) a else b)

  // Generic, for non-Evaluation classes
  def apply[T](set: Seq[T], better: (T, T) => Boolean) =
    set.reduceLeft((a, b) => if (better(a, b)) a else b)

  def apply[T](set: Seq[T], ord: Ordering[T]) =
    set.reduceLeft((a, b) => if (ord.compare(a, b) < 0) a else b)
}

object TestBestSelector {
  def main(args: Array[String]) =
    println(BestSelector(List(3, 1, 3, 6), Ordering[Int]))
}