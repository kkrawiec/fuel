package scevo.func

import scevo.util.Collector
import scevo.util.Options
import scevo.core.StatePop

/** Simple helper function for maintaining the best-so-far solutions 
 *  
 *  Can be plugged anywhere in the algorithm workflow. 
 * Does also simple snapshots, i.e., saves the current state of Collector.   
 */

class BestSoFar[S, E](opt: Options, coll: Collector, o: Ordering[E])
    extends Function1[StatePop[(S, E)], StatePop[(S, E)]] {
  protected var best: Option[(S, E)] = None
  def bestSoFar = best
  val snapFreq = opt("snapshot-frequency", 0)

  def apply(s: StatePop[(S, E)]) = {
    val bestOfGen = s.solutions.minBy(_._2)(o)
    if (bestSoFar.isEmpty || o.lt(bestOfGen._2, best.get._2)) best = Some(bestOfGen)
    println(f"Gen: ${s.iteration}  BestSoFar: ${bestSoFar.get}")
    if (snapFreq > 0 && s.iteration % snapFreq == 0) {
      updateBest(s)
      coll.saveSnapshot(f"${s.iteration}%04d")
    }
    s
  }
  def updateBest(state: StatePop[(S, E)]) = {
    coll.setResult("lastGeneration", state.iteration)
    coll.setResult("bestOfRun.eval",
      if (bestSoFar.isDefined) bestSoFar.get._2 else "NaN")
    coll.setResult("bestOfRun", bestSoFar.toString)
    coll.write("bestOfRun", bestSoFar)
    state
  }
}
object BestSoFar {
  def apply[S, E](o: Ordering[E])(implicit opt: Options, coll: Collector) =
    new BestSoFar[S, E](opt, coll, o)
}


