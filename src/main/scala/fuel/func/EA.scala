package fuel.func

import fuel.core.StatePop
import fuel.moves.Moves
import fuel.util.CallCounter
import fuel.util.CallEvery
import fuel.util.Collector
import fuel.util.TRandom
import fuel.util.Options

trait IterativeSearch[S] extends Function1[S, S] {
  def iter: S => S
  def terminate: Seq[S => Boolean]
  protected val it = CallCounter(identity[S])
  def algorithm = Iteration(iter andThen it)(terminate)
  def apply(s: S) = algorithm(s)
}

/**
  * Core implementation of evolutionary algorithm, i.e. iterative population-based search.
  *
  *  Note: there is no assumption of any ordering of solutions (complete nor partial).
  *  Because of that, it is in general impossible to monitor progress, hence report and
  *  epilogue are stubs.
  *
  * Uses parallel evaluation (number of threads set automatically).
  * All solutions are considered feasible.
  * Environment (options and collector) passed automatically as implicit parameters.
  *
  * Technically, EACore is both Function0[State] as well as Function1[State,State], so
  * it may be used to either start from scratch (in the former case) or be applied
  * to some already existing State.
  *
  */
abstract class EACore[S, E](moves: Moves[S], evaluation: Evaluation[S, E],
                            stop: (S, E) => Boolean = ((s: S, e: E) => false))(
                              implicit opt: Options)
    extends IterativeSearch[StatePop[(S, E)]] with Function0[StatePop[(S, E)]] {

  /*
  def this(moves: Moves[S], eval: S => E,
           stop: (S, E) => Boolean = ((s: S, e: E) => false))(
             implicit opt: Options) = this(moves, ParallelEval(eval), stop)(opt)
             * 
             */

  def initialize: Unit => StatePop[(S, E)] = RandomStatePop(moves.newSolution _) andThen evaluate andThen it
  def evaluate = evaluation andThen report 
  override def terminate = Termination(stop).+:(Termination.MaxIter(it))
  def report = (s: StatePop[(S, E)]) => { println(f"Gen: ${it.count}"); s }
  def apply() = (initialize andThen algorithm)()
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
    extends EACore[S, E](moves,
      if (opt('parEval, true)) ParallelEval(eval) else SequentialEval(eval),
      stop)(opt) {

  def selection : Selection[S,E] = TournamentSelection[S, E](ordering)
  override def iter = SimpleBreeder[S, E](selection, moves: _*) andThen evaluate

  val bsf = BestSoFar[S, E](ordering, it)
  override def report: Function1[StatePop[(S, E)], StatePop[(S, E)]] = bsf
}

object SimpleEA {
  def apply[S, E](moves: Moves[S], eval: S => E)(
    implicit opt: Options, coll: Collector, rng: TRandom, ordering: Ordering[E]) =
    new SimpleEA(moves, eval)(opt, coll, rng, ordering)

  def apply[S, E](moves: Moves[S], eval: S => E, stop: (S, E) => Boolean)(
    implicit opt: Options, coll: Collector, rng: TRandom, ordering: Ordering[E]) =
    new SimpleEA(moves, eval, stop)(opt, coll, rng, ordering)

  /** Creates EA that should stop when evaluation reaches certain value */
  def apply[S, E](moves: Moves[S], eval: S => E, optimalValue: E)(
    implicit opt: Options, coll: Collector, rng: TRandom, ordering: Ordering[E]) =
    new SimpleEA(moves, eval, ((_: S, e: E) => e == optimalValue))(opt, coll, rng, ordering)
}

/**
  * Simple steady-state EA, with reverse tournament selection
  * for deselection of bad solutions.
  *
  */
class SimpleSteadyStateEA[S, E](moves: Moves[S],
                                eval: S => E,
                                stop: (S, E) => Boolean = ((s: S, e: E) => false))(
                                  implicit opt: Options, coll: Collector, rng: TRandom, ordering: Ordering[E])
    extends SimpleEA[S, E](moves, eval, stop)(opt, coll, rng, ordering) {

  val n = opt('populationSize, 1000)
  val deselection = TournamentSelection[S, E](ordering.reverse)
  override def iter = new SimpleSteadyStateBreeder[S, E](selection,
    RandomMultiOperator(moves: _*), deselection, eval) andThen CallEvery(n, report)
}

object SimpleSteadyStateEA {
  def apply[S, E](moves: Moves[S], eval: S => E, stop: (S, E) => Boolean)(
    implicit opt: Options, coll: Collector, rng: TRandom, ordering: Ordering[E]) =
    new SimpleSteadyStateEA(moves, eval, stop)(opt, coll, rng, ordering)

  def apply[S, E](moves: Moves[S], eval: S => E)(
    implicit opt: Options, coll: Collector, rng: TRandom, ordering: Ordering[E]) =
    new SimpleSteadyStateEA(moves, eval)(opt, coll, rng, ordering)
}
