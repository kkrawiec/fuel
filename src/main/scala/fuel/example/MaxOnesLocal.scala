package fuel.example

import fuel.moves.BoolVectNeigh
import fuel.util.OptColl
import fuel.func.RunExperiment
import fuel.func.LocalSteepest

/**
  * Using local search for MaxOnes (see the MaxOnesExample)
  *
  */
object MaxOnesLocal extends App {
  new OptColl('numVars -> 20, 'maxGenerations -> 200, 'printResults -> true) {
    val n = opt('numVars, (_: Int) > 0)
    val neigh = new BoolVectNeigh
    // Initial search state
    val init = IndexedSeq.fill(n)(true)
    RunExperiment(Unit => {
      val alg = new LocalSteepest(neigh,
        eval = (s: IndexedSeq[Boolean]) => s.count(_ == true))
      val bestOfRun = alg.apply(init)
      println("BestOfRun: " + bestOfRun)
    })
  }
}
