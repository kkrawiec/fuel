package scevo.func

import scevo.util.Options

import scevo.util.Collector
import scevo.util.TRandom
import scevo.moves.Moves
import scevo.core.Dominance
import scevo.core.StatePop

/**
  * Generic trait for population-based iterative (parallel) search
  *
  */
trait IterativeSearch[S, E] extends Function1[Unit, StatePop[(S, E)]] {
  type ST = StatePop[S] // Population/State of non-evaluated solutions
  type SE = StatePop[(S, E)] // Population/State of evaluated solutions
  def initialize: Unit => ST
  def evaluate: ST => SE
  def breed: SE => ST
  def terminate: Seq[SE => Boolean]
  def algorithm = initialize andThen evaluate andThen Iteration(breed andThen evaluate)(terminate) 
  def apply(x: Unit) = algorithm()
}

/**
  * Core implementation of evolutionary algorithm.
  *
  *  Note: there is no assumption of any ordering of solutions (complete nor partial).
  *  Because of that, it is in general impossible to monitor progress, hence report and
  *  epilogue are stubs.
  *
  * Uses parallel evaluation (number of threads set automatically). 
  * All solutions are considered feasible.
  * Environment (options and collector) passed automatically as implicit parameters.
  *
  * TODO: if stop() is default, it should not be called
  */
abstract class EACore[S, E](moves: Moves[S],
                            eval: S => E,
                            stop: (S, E) => Boolean = ((s: S, e: E) => false))(
                              implicit opt: Options)
    extends IterativeSearch[S, E] {

  override def initialize = RandomStatePop(moves.newSolution _)
  override def evaluate = ParallelEval(eval) andThen report
  override def terminate = Termination(stop)
  def report = (s: SE) => { println(f"Gen: ${s.iteration}"); s }
}

/**
  * Simple, default implementation of generational evolutionary algorithm.
  *
  * Assumes complete ordering of candidate solutions (Ordering).
  * For complete orders, it is also clear how to find the BestSoFar solution.
  *
  */
class SimpleEA[S, E](moves: Moves[S],
                     eval: S => E,
                     stop: (S, E) => Boolean = ((s: S, e: E) => false))(
                       implicit opt: Options, coll: Collector, rng: TRandom, ordering: Ordering[E])
    extends EACore[S, E](moves, eval, stop)(opt) {

  def selection = TournamentSelection[S, E](ordering)
  override def breed = SimpleBreeder[S, E](selection, RandomMultiOperator(moves.moves: _*))

  val bsf = BestSoFar[S, E](ordering)
  override def report = bsf
  override def algorithm = super.algorithm andThen EpilogueBestOfRun(bsf)
}

object SimpleEA {
  def apply[S, E](moves: Moves[S],
                  eval: S => E,
                  stop: (S, E) => Boolean = ((s: S, e: E) => false))(
                    implicit opt: Options, coll: Collector, rng: TRandom, ordering: Ordering[E]) = new SimpleEA(moves, eval, stop)(opt, coll, rng, ordering)
}
 