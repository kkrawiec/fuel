package scevo.evo

import scevo.tools.Randomness

/*
 * A single step of an iterative search algorithm. apply() is supposed to carry out one iteration. 
 * A single step (apply()) may include feasibility test, so it may be unsuccessfull, hence Option. 
 * A search operator returns a list of of candidate solutions; possibly empty (if, e.g., feasibility conditions are not met).
 */
//trait SearchStep[S <: Solution, ES <: EvaluatedSolution[E], E <: Evaluation]{
trait SearchStep[S <: State] {
  def apply(history: Seq[S]): Option[S]
}

trait SearchStepStochastic[S <: Solution, E <: Evaluation]
  extends SearchStep[PopulationState[S,E]] {
  this: StochasticSearchOperators[S,E] 
  with Selection[S,E] with Evaluator[S, E] with Randomness =>
  /*
   * history is the list of previous search states, with the most recent one being head. 
   * In most cases, it is only the most recent state (keeping entire history may be too memory costly). 
   */
  override def apply(history: Seq[PopulationState[S,E]]): Option[PopulationState[S,E]] = {
    require(history.nonEmpty)
    val source = selector(history)
    var offspring = scala.collection.mutable.MutableList[S]()
    // Note: This loop will iterate forever is none of the search operators manages to produce a solution. 
    while (offspring.size < source.numSelected)
      offspring ++= operator(rng)(source)
    val evaluated = apply(offspring.toList)
    Some(PopulationState(evaluated, history.head.iteration + 1))
  }
}

