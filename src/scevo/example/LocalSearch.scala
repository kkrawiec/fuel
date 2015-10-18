package scevo.example

import scevo.moves.BoolVectNeigh
import scevo.util.OptColl
import scevo.func.RunExperiment
import scevo.func.LocalSearch

/**
  * Using local search for MaxOnes (see the MaxOnesExample)
  *
  * TODO: BestOfRun
  */
object MaxOnesLocal extends App {
  new OptColl('numVars -> 500, 'maxGenerations -> 200, 'printResults -> true) {
    val n = opt('numVars, (_: Int) > 0)
    val neigh = new BoolVectNeigh
    // Initial search state
    val init = IndexedSeq.fill(n)(true)
    RunExperiment(Unit => {
      val alg = new LocalSearch(neigh,
        eval = (s: IndexedSeq[Boolean]) => s.count(_ == true))
      alg.apply(init)
    })
  }
}
