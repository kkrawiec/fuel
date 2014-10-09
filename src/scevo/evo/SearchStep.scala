package scevo.evo

import scevo.tools.TRandom

/*
 * Iterative search algorithm. apply() is supposed to carry out one iteration. 
 * A single step (apply()) may include feasibility test, so it may be unsuccessfull, hence Option. 
 * A search operator returns a list of of candidate solutions; possibly empty (if, e.g., feasibility conditions are not met).
 */
trait SearchStep[ES <: EvaluatedSolution[_ <: Evaluation]] {
  def apply( history : Seq[State[ES]]) : Option[State[ES]]
}


class SearchStepWithEval[S <: Solution, ES <: EvaluatedSolution[E], E <: Evaluation](
  val searchOperators: Seq[(Selector[ES,E] => Seq[S], Double)],
  val evalFunction: Seq[S] => Seq[ES],
  val selection: Selection[ES,E],
  val rng: TRandom)
  extends SearchStep[ES]  {

  assert(searchOperators.nonEmpty, "At least one search operator should be declared")
  assert(searchOperators.map(_._2).sum == 1, "Operators' probabilities should sum up to 1.0")
  assert(searchOperators.forall(_._2 >= 0), "Operators' probabilities cannot be negative.")

  /*
   * history is the list of previous search states, with the most recent one being head. 
   * In most cases, it is only the most recent state (keeping entire history may be too memory costly). 
   */
  def apply(history: Seq[State[ES]]): Option[State[ES]] = {
    require(history.nonEmpty)

    val source = selection.selector(history)

    var offspring = scala.collection.mutable.MutableList[S]()
    // Note: This loop will iterate forever is non of the search operators manages to produce a solution. 
    while (offspring.size < source.numSelected) {
      val r = rng.nextDouble
      var sum: Double = 0
      offspring ++= searchOperators.find(e => { sum += e._2; sum >= r }).get._1(source)
    }
    // Evaluation of an individual may end with None, which signals infeasible solution
//    val evaluated = offspring.toIndexedSeq.map(evalFunction(_)).flatten
    val evaluated = evalFunction( offspring.toList )
    // parallel version: val evaluated = current.solutions.par.map(evalFunction(_, current)).flatten.seq
    if (evaluated.isEmpty)
      None // In case no individual passed the evaluation stage
    else
      Some(State[ES](evaluated, history.head.iteration + 1))
  }

}

