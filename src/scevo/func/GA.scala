package scevo.func

import scevo.tools.Options
import scevo.tools.Collector
import scevo.tools.TRandom

/**
  * Default implementation of Genetic Algorithm (GA).
  *
  * Uses parallel evaluation
  * All solutions are considered feasible.
  * Environment (options and collector) passed automatically as implicit parameters.
  *
  *
  *
  */

class SimpleGA[S, E](domain: GADomain[S], eval: S => E, stop: (S, E) => Boolean)(
  implicit opt: Options, coll: Collector, rng: TRandom, ordering: Ordering[E])
    extends Function1[Unit, StatePop[(S, E)]] {

  def initialize = RandomStatePop(domain.randomSolution _)

  def evaluate = ParallelEval(eval)

  def breed = SimpleBreeder[S, E](
    TournamentSelection(ordering),
    RandomMultiOperator(domain.oneBitMutation, domain.onePointCrossover))

  def terminate = Termination(stop)

  def report = BestSoFar[S, E](ordering)

  def algorithm = initialize andThen evaluate andThen
    Iteration(breed andThen evaluate andThen report)(terminate) andThen
    EpilogueBestOfRun(report)

  def apply(x: Unit) = algorithm()
}