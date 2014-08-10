package scevo.evo

/*
 * Represents a non-evaluated solution
 */

trait Solution {

}

trait Fitness {

}
/*
 * Represents an evaluated solution. 
 * Only evaluated solutions can undergo selection. 
 * Search operators take evaluated solutions as arguments and produce non-evaluated solutions.
 */

trait EvaluatedSolution extends Solution with Fitness {

  // Returns None in case of incomparability
  def betterThan( other : EvaluatedSolution ) : Option[Boolean]
 
  def fitness : Double

}