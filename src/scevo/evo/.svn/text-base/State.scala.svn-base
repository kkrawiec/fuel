package scevo.evo


class State[S <: Solution]( val solutions : Seq[S] ) {
  
    assert( solutions.size > 0, "The set of working solutions in a state cannot be empty" )
//  def getSolutions : Seq[Solution]

}


object State {

  def apply[S <: Solution]( popSize : Int, genSolution: => S ) : State[S] = {
    assert( popSize > 0, "Population cannot be empty" )

	new State( for( i <- 0 until popSize) yield genSolution )
  }

}