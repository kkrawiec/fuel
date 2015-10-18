package scevo.func

import scevo.util.Collector
import scevo.util.Options
import scevo.core.StatePop
import scevo.util.Counter

/** Simple helper function for maintaining the best-so-far solutions 
 *  
 *  Can be plugged anywhere in the algorithm workflow. 
 * Does also simple snapshots, i.e., saves the current state of Collector.   
 */

class BestSoFar[S, E](opt: Options, coll: Collector, o: Ordering[E], cnt: Counter)
    extends Function1[StatePop[(S, E)], StatePop[(S, E)]] {
  protected var best: Option[(S, E)] = None
  def bestSoFar = best
  val snapFreq = opt("snapshot-frequency", 0)

  def apply(s: StatePop[(S, E)]) = {
    val bestOfGen = s.minBy(_._2)(o)
    if (bestSoFar.isEmpty || o.lt(bestOfGen._2, best.get._2)) best = Some(bestOfGen)
    println(f"Gen: ${cnt.count}  BestSoFar: ${bestSoFar.get}")
    if (snapFreq > 0 && cnt.count % snapFreq == 0) {
      updateBest(s)
      coll.saveSnapshot(f"${cnt.count}%04d")
    }
    s
  }
  def updateBest(state: StatePop[(S, E)]) = {
    coll.setResult("lastGeneration", cnt.count)
    coll.setResult("bestOfRun.eval",
      if (bestSoFar.isDefined) bestSoFar.get._2 else "NaN")
    coll.setResult("bestOfRun", bestSoFar.toString)
    coll.write("bestOfRun", bestSoFar)
    state
  }
}
object BestSoFar {
  def apply[S, E](o: Ordering[E], cnt: Counter)(implicit opt: Options, coll: Collector) =
    new BestSoFar[S, E](opt, coll, o, cnt)
}


