package scevo.evo

import scevo.tools.Randomness

/*
 * Iterative search algorithm. apply() is supposed to carry out one iteration. 
 * A single step (apply()) may include feasibility test, so it may be unsuccessfull, hence Option. 
 * A search operator returns a list of of candidate solutions; possibly empty (if, e.g., feasibility conditions are not met).
 */
//trait SearchStep[S <: Solution, ES <: EvaluatedSolution[E], E <: Evaluation]{
trait SearchStep[S <: State]{
//  this : SearchOperators[ES,E,S] =>
  def apply( history : Seq[S]) : Option[S]
}


trait SearchStepStochastic[S <: Solution, ES <: EvaluatedSolution[_]]
  extends SearchStep[PopulationState[ES]]  {
  this : StochasticSearchOperators[ES,S]
  with Selection[ES] with Evaluator[S,ES] with Randomness =>
  /*
   * history is the list of previous search states, with the most recent one being head. 
   * In most cases, it is only the most recent state (keeping entire history may be too memory costly). 
   */
  override def apply(history: Seq[PopulationState[ES]]): Option[PopulationState[ES]] = {
    require(history.nonEmpty)

    val source = selector(history)

    var offspring = scala.collection.mutable.MutableList[S]()
    // Note: This loop will iterate forever is non of the search operators manages to produce a solution. 
    while (offspring.size < source.numSelected) 
      offspring ++= operator(rng)(source)
    // Evaluation of an individual may end with None, which signals infeasible solution
//    val evaluated = offspring.toIndexedSeq.map(evalFunction(_)).flatten
    val evaluated = apply( offspring.toList )
    // parallel version: val evaluated = current.solutions.par.map(evalFunction(_, current)).flatten.seq
    if (evaluated.isEmpty)
      None // In case no individual passed the evaluation stage
    else
      Some(PopulationState[ES](evaluated, history.head.iteration + 1))
  }
}

