package scevo.func

import scevo.tools.Options
import scevo.tools.Collector
import scevo.tools.TRandom
import scevo.domain.Moves
import scevo.evo.Dominance
import scevo.evo.StatePop

/**
  * Default implementation of Evolutionary Algorithm (EA).
  *
  * Uses parallel evaluation
  * All solutions are considered feasible.
  * Environment (options and collector) passed automatically as implicit parameters.
  *
  * Uses PartialOrdering.
  *
  * TODO: if stop() is default, it should not be called
  */

abstract class EA[S, E](moves: Moves[S],
                        eval: S => E,
                        stop: (S, E) => Boolean = ((s: S, e: E) => false))(
                          implicit opt: Options, coll: Collector, rng: TRandom, ordering: PartialOrdering[E])
    extends Function1[Unit, StatePop[(S, E)]] {

  type Step = Function1[StatePop[(S, E)], StatePop[(S, E)]]

  def initialize = RandomStatePop(moves.newSolution _)

  def evaluate = ParallelEval(eval)

  def breed: Breeder[S, E]

  def terminate = Termination(stop)

  val bsf = BestSoFar[S, E](ordering)
  def report = bsf

  def epilogue: Step = EpilogueBestOfRun(bsf.bestSoFar)

  def algorithm = initialize andThen evaluate andThen
    Iteration(breed andThen evaluate andThen report)(terminate) andThen epilogue

  def apply(x: Unit) = algorithm()
}

/**
  * For complete Ordering.
  *
  */
class SimpleEA[S, E](moves: Moves[S],
                     eval: S => E,
                     stop: (S, E) => Boolean = ((s: S, e: E) => false))(
                       implicit opt: Options, coll: Collector, rng: TRandom, ordering: Ordering[E])
    extends EA[S, E](moves, eval, stop)(opt, coll, rng, ordering) {

  def selection = TournamentSelection[S, E](ordering)
  override def breed = SimpleBreeder[S, E](selection, RandomMultiOperator(moves.moves: _*))
}

object SimpleEA {
  def apply[S, E](moves: Moves[S],
                  eval: S => E,
                  stop: (S, E) => Boolean = ((s: S, e: E) => false))(
                    implicit opt: Options, coll: Collector, rng: TRandom, ordering: Ordering[E]) = new SimpleEA(moves, eval, stop)(opt, coll, rng, ordering)
}
 