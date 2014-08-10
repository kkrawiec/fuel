package scevo.evo

import scevo.tools.TRandom

/*
 * Iterative search algorithm. apply() is supposed to carry out one iteration. 
 */
trait SearchAlgorithm[S <: Solution, ES <: EvaluatedSolution]
  extends ((State[S], Seq[ES]) => Option[(State[S], Seq[ES])])

class SearchAlgorithmWithEval[S <: Solution, ES <: EvaluatedSolution](
  val searchOperators: Seq[(ES => S, Double)],
  val evalFunction: S => Option[ES],
  val selection: Selection[ES], // (Seq[ES], Int, Seq[ES]) => Seq[ES],
  val rng: TRandom)
  extends SearchAlgorithm[S, ES] {

  assert(searchOperators.length > 0, "At least one search operator should be declared")
  assert(searchOperators.map(_._2).sum == 1, "Operators' probabilities should sum up to 1.0")
  assert(searchOperators.forall(_._2 >= 0), "Operators' probabilities cannot be negative.")

  def apply(current: State[S], previous: Seq[ES] = Seq[ES]()): Option[(State[S], Seq[ES])] = {
    // Evaluation of an individual may end with None, which signals infeasible solution
    val evaluated = current.solutions.map(evalFunction(_)).flatten
    // parallel version: val evaluated = current.solutions.par.map(evalFunction(_, current)).flatten.seq
    if (evaluated.isEmpty) // In case no individual passed the evaluation stage
      // This is really a problem; can occur also at the first step of search. TODO: will need to move search operators to the beginning of this function. 
      None
    else {
      //println( "To select: " + current.solutions.length )
      val selected = selection(evaluated, current.solutions.length, previous)
      //println( "Selected: " + selected.length )
      val offspring = selected.map({
        val r = rng.nextDouble
        var sum: Double = 0
        searchOperators.find(e => { sum += e._2; sum >= r }).get._1(_)
      })
      Some(new State(offspring), evaluated)
    }
  }

}

