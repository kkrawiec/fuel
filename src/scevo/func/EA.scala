package scevo.func

import scevo.tools.Options
import scevo.tools.Collector
import scevo.tools.TRandom
import scevo.moves.Moves
import scevo.evo.Dominance
import scevo.evo.StatePop

/** Core implementation of iterative parallel (i.e., population-based) search. 
 *  
 *  Note: there is no assumption of any ordering of solutions (complete nor partial). 
 *  Because of that, it is in general impossible to monitor progress, hence report and
 *  epilogue are stubs.  
 *  
  * Uses parallel evaluation
  * All solutions are considered feasible.
  * Environment (options and collector) passed automatically as implicit parameters.
  * 
  * TODO: if stop() is default, it should not be called
  */
abstract class EACore[S, E](moves: Moves[S],
                            eval: S => E,
                            stop: (S, E) => Boolean = ((s: S, e: E) => false))(
                              implicit opt: Options)
    extends Function1[Unit, StatePop[(S, E)]] {

  def initialize = RandomStatePop(moves.newSolution _)
  def evaluate = ParallelEval(eval)
  def breed: StatePop[(S, E)] => StatePop[S] //Breeder[S, E]
  def terminate = Termination(stop)
  def report = (s : StatePop[(S,E)]) => { println(f"Gen: ${s.iteration}"); s }
  def epilogue = identity[StatePop[(S, E)]] _
  def algorithm = initialize andThen evaluate andThen
    Iteration(breed andThen evaluate andThen report)(terminate) andThen epilogue
  def apply(x: Unit) = algorithm()
}

/**
  * Default implementation of Evolutionary Algorithm (EA).
  *
  * Uses PartialOrdering.
  *
  */
abstract class EA[S, E](moves: Moves[S],
                        eval: S => E,
                        stop: (S, E) => Boolean = ((s: S, e: E) => false))(
                          implicit opt: Options, coll: Collector, rng: TRandom, ordering: PartialOrdering[E])
    extends EACore[S, E](moves, eval, stop)(opt) {

  val bsf = BestSoFar[S, E](ordering)
  override def report = bsf
  override def epilogue: Function1[StatePop[(S, E)], StatePop[(S, E)]] = EpilogueBestOfRun(bsf)
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
 