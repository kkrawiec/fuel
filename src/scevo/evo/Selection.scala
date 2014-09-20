package scevo.evo


import scevo.tools.TRandom

/* Selector is intended to operate in two phases: 
 * 1. When created, it can prepare helper data structures (or perform 'batch selection', as NSGAII does)
 * 2. Then, single applications of next() should return selected individuals. 
 * next() should never fail, because an algorithm may need to call it more than numSelected times
 */

trait Selector[ES <: EvaluatedSolution[E], E <: Evaluation] {
  def next: ES
  def numSelected: Int
}

trait Selection[ES <: EvaluatedSolution[E], E <: Evaluation] {
  def selector(history: Seq[State[ES]]): Selector[ES,E]
}

class TournamentSelection[ES <: EvaluatedSolution[E], E <: Evaluation] (val tournSize: Int, rng: TRandom)
  extends Selection[ES,E] {
  require(tournSize >= 2, "Tournament size has to be at least 2")
  override def selector(history: Seq[State[ES]]) = new Selector[ES,E] {
    private val pool = history.head.solutions
    override val numSelected = pool.size
    override def next: ES =
      BestSelector(for (i <- 0 until tournSize) yield pool(rng.nextInt(pool.size))).asInstanceOf[ES]
  }
}

class MuLambdaSelection[ES <: EvaluatedSolution[E], E <: Evaluation]
  extends Selection[ES,E] {
  override def selector(history: Seq[State[ES]]) = new Selector[ES,E] {
    val pool = if (history.size == 1)
      history.head.solutions
    else
      history.head.solutions ++ history.tail.head.solutions
    private val selected = pool.sortWith((a, b) => a.eval.betterThan(b.eval))
    override val numSelected = history.head.solutions.size
    private var i = -1
    override def next: ES = {
      i = (i + 1) % numSelected
      selected(i)
    }
  }
}

class GreedyBestSelection[ES <: EvaluatedSolution[E], E <: Evaluation] 
extends Selection[ES,E] {
  override def selector(history: Seq[State[ES]]) = new Selector[ES,E] {
    override val numSelected = 1
    override val next: ES = BestSelector(history.head.solutions).asInstanceOf[ES]
  }
}

object BestSelector {
  def apply(set: Seq[EvaluatedSolution[Evaluation]]): EvaluatedSolution[Evaluation] = {
    require(set.nonEmpty)
    var best = set.head
    set.tail.foreach(e => if (e.eval.betterThan(best.eval)) best = e)
    best
  }
}
