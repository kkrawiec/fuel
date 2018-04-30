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
  * Can use parallel evaluation (number of threads set automatically) or sequential evaluation
  * (only one thread).
  * All solutions are considered feasible.
  * Environment (options and collector) passed automatically as implicit parameters.
  *
  * Technically, EACore is both Function0[State] as well as Function1[State,State], so
  * it may be used to either start from scratch (in the former case) or be applied
  * to some already existing State.
  *
  */
abstract class EACore[S, E](moves: Moves[S],
                            evaluation: Evaluation[S, E],
                            stop: (S, E) => Boolean = ((s: S, e: E) => false))
                           (implicit opt: Options)
    extends IterativeSearch[StatePop[(S, E)]] with Function0[StatePop[(S, E)]] {

  def initialize: Unit => StatePop[(S, E)] = RandomStatePop(moves.newSolution _) andThen evaluate andThen it
  def evaluate = evaluation andThen report 
  override def terminate = Termination(stop).+:(Termination.MaxIter(it))
  def report = (s: StatePop[(S, E)]) => { println(f"Gen: ${it.count}"); s }
  def apply(): StatePop[(S, E)] = epilogue((initialize andThen algorithm)())
  def epilogue: StatePop[(S, E)] => StatePop[(S, E)] = s => { println("Run finished."); s}
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
                     stop: (S, E) => Boolean = ((s: S, e: E) => false))
                    (implicit opt: Options, coll: Collector, rng: TRandom, ordering: Ordering[E])
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
  * More general version of a steady-state EA, in which selection and deselection may
  * be arbitrary.
  *
  * SteadyStateEA during a single iteration in this order:
  * 1) Generates exactly one new offspring and evaluates it using the provided eval function.
  *    Parents are chosen by the selection function.
  * 2) Removes from the population the individual chosen by deselection function. Position of
  *    the removal is remembered.
  * 3) Inserts the offspring generated in 1) at the position of the removal.
  */
class SteadyStateEA[S, E](moves: Moves[S],
                    eval: S => E,
                    stop: (S, E) => Boolean = ((s: S, e: E) => false),
                    selection: Selection[S, E],
                    deselection: Selection[S, E])
                    (implicit opt: Options, coll: Collector, rng: TRandom, ordering: Ordering[E])
    extends EACore[S, E](moves,
                         if (opt('parEval, true)) ParallelEval(eval) else SequentialEval(eval),
                         stop)(opt) {
  val n = opt('reportFreq, opt('populationSize, 1000))
  override def iter = new SimpleSteadyStateBreeder[S, E](selection,
    RandomMultiOperator(moves: _*), deselection, eval) andThen CallEvery(n, report)
  
  val bsf = BestSoFar[S, E](ordering, it)
  override def report: (StatePop[(S, E)]) => StatePop[(S, E)] = bsf
}


/**
  * Simple steady-state EA, with reverse tournament selection for deselection of bad
  * solutions and tournament selection of good solutions.
  *
  */
class SimpleSteadyStateEA[S, E](moves: Moves[S],
                                eval: S => E,
                                stop: (S, E) => Boolean = ((s: S, e: E) => false))
                               (implicit opt: Options, coll: Collector, rng: TRandom, ordering: Ordering[E])
    extends SteadyStateEA[S, E](moves, eval, stop,
                                TournamentSelection[S, E](ordering),
                                TournamentSelection[S, E](ordering.reverse))(opt, coll, rng, ordering) {
}

object SimpleSteadyStateEA {
  def apply[S, E](moves: Moves[S], eval: S => E, stop: (S, E) => Boolean)
                 (implicit opt: Options, coll: Collector, rng: TRandom, ordering: Ordering[E]) =
    new SimpleSteadyStateEA(moves, eval, stop)(opt, coll, rng, ordering)

  def apply[S, E](moves: Moves[S], eval: S => E)
                 (implicit opt: Options, coll: Collector, rng: TRandom, ordering: Ordering[E]) =
    new SimpleSteadyStateEA(moves, eval)(opt, coll, rng, ordering)
}



/**
  * At each iteration, MultipopulationEA proceeds in the following way:
  * 1) Populations are sequentially evolved using the evolution object provided by eaCreator.
  *    Stop conditions in that object determine the stop of the evolution in the currently
  *    considered subpopulation.
  * 2) A list of populations is passed to the popsDivide function, which may redistrubute
  *    solutions, create/remove populations etc. For example in the island model, some
  *    solutions can be exchanged between different populations.
  * The above steps are repeated until the provided stop conditions of the multi-population
  * EA scheme are met.
  *
  * @param popsDivide takes the current sequence of populations and creates a new
  *                   sequence of populations.
  * @param eaCreator a function to create a new search algorithm (e.g. EA) to be applied to
  *                  a given population. It is created anew in order to avoid mutability.
  * @param maxIter maximum number of iterations.
  * @param maxTime maximum runtime in miliseconds.
  * @param stop custom stop condition, e.g. optimal solution found in one of the subpopulations.
  * @tparam S type of solution object.
  * @tparam E type of evaluation object.
  */
class MultipopulationEA[S,E]
      (val popsDivide: Seq[StatePop[(S,E)]] => Seq[StatePop[(S,E)]],
       val eaCreator: () => EACore[S,E],
       val maxIter: Option[Int] = Some(100),
       val maxTime: Option[Int] = Some(86400000),
       val stop: StatePop[(S, E)] => Boolean = (s: StatePop[(S, E)]) => false)
      extends IterativeSearch[Seq[StatePop[(S,E)]]]() with Function0[Seq[StatePop[(S, E)]]] {

  override def iter: Seq[StatePop[(S,E)]] => Seq[StatePop[(S,E)]] =
    popsEvolve andThen popsDivide andThen report //andThen printPops

  def apply(): Seq[StatePop[(S, E)]] =
    epilogue((eaCreator().initialize andThen (s => Seq(s)) andThen //printPops andThen
      popsDivide andThen algorithm)())

  def report: Seq[StatePop[(S, E)]] => Seq[StatePop[(S, E)]] =
    (s: Seq[StatePop[(S, E)]]) => s

  def epilogue: Seq[StatePop[(S, E)]] => Seq[StatePop[(S, E)]] =
    (s: Seq[StatePop[(S, E)]]) => s

  /**
    * Runs sequentially a local evolution of current populations.
    */
  def popsEvolve: Seq[StatePop[(S, E)]] => Seq[StatePop[(S, E)]] =
    (pops: Seq[StatePop[(S,E)]]) => pops.map{ pop => eaCreator().apply(pop) }

  /**
    * Set of termination functions.
    */
  override def terminate: Seq[Seq[StatePop[(S,E)]] => Boolean] = {
    val stopFun = (pops: Seq[StatePop[(S,E)]]) => pops.exists(stop(_))
    (if (maxIter.isDefined) Seq(Termination.MaxIter(it, maxIter.get)) else Seq()) ++
    (if (maxIter.isDefined) Seq(Termination.MaxTime(maxTime.get)) else Seq()) ++
    Seq(stopFun)
  }

  def printPops(pops: Seq[StatePop[(S,E)]]): Seq[StatePop[(S,E)]] = {
    pops.indices.toList.zip(pops).foreach { case (i, pop) =>
      println(s"POPULATION $i")
      for (x <- pop) println(x)
      println()
    }
    pops
  }
}


object MultipopulationEA {
  /**
    * Convection multi-population EA scheme in the EqualNumber variant as described in [1], [2].
    * M populations are independently evolving, and after a certain number of iterations
    * solutions from all populations are globally sorted on their fitnesses and divided
    * again into M equally-sized populations with decreasing fitness values.
    *
    * There is no overlapping between fitness ranges used to divide solutions into populations.
    *
    * [1] M. Komosinski, K. Miazga, "Tournament-Based Convection Selection in Evolutionary
    *     Algorithms", 2018, Parallel Processing and Applied Mathematics.
    * [2] M. Komosinski, K. Miazga, "Comparison of the tournament-based convection selection
    *     with the island model in evolutionary algorithms", 2018, submitted.
    *
    * @param pops sequence of populations.
    * @param M number of subpopulations.
    * @tparam S type of solution object.
    * @tparam E type of evaluation object.
    * @return
    */
  def convectionEqualNumber[S,E](M: Int, ordering: Ordering[E])
                                (pops: Seq[StatePop[(S,E)]]): Seq[StatePop[(S,E)]] = {
    // Merge all populations and sort the solutions on the fitness value
    val sPop = pops.flatten.sortBy(_._2)(ordering)
    // Divide on M subpopulations and return them
    sPop.grouped(sPop.size / M).toList.map(StatePop(_))
  }
}