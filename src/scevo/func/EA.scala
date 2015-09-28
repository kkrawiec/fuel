package scevo.func

import scevo.tools.Options
import scevo.tools.Collector
import scevo.tools.TRandom
import scevo.domain.Moves
import scevo.domain.Domain

/**
  * Default implementation of Genetic Algorithm (GA).
  *
  * Uses parallel evaluation
  * All solutions are considered feasible.
  * Environment (options and collector) passed automatically as implicit parameters.
  *
  *
  * TODO: if stop() is default, it should not be called
  */

class SimpleEA[S, E](domain: Domain[S] with Moves[S],
                     eval: S => E,
                     stop: (S, E) => Boolean = ((s: S, e: E) => false))(
                       implicit opt: Options, coll: Collector, rng: TRandom, ordering: Ordering[E])
    extends Function1[Unit, StatePop[(S, E)]] {

  def initialize = RandomStatePop(domain.randomSolution _)

  def evaluate = ParallelEval(eval)

  def breed = SimpleBreeder[S, E](TournamentSelection(ordering),
    RandomMultiOperator(domain.moves: _*))

  def terminate = Termination(stop)

  def report = BestSoFar[S, E](ordering)

  def algorithm = initialize andThen evaluate andThen
    Iteration(breed andThen evaluate andThen report)(terminate) andThen
    EpilogueBestOfRun(report)

  def apply(x: Unit) = algorithm()
}

object SimpleEA {
  def apply[S, E](domain: Domain[S] with Moves[S],
                  eval: S => E,
                  stop: (S, E) => Boolean = ((s: S, e: E) => false))(
                    implicit opt: Options, coll: Collector, rng: TRandom, ordering: Ordering[E]) 
                    = new SimpleEA(domain, eval, stop)(opt, coll, rng, ordering)
}
 