package scevo.func

import scala.collection.parallel.ForkJoinTaskSupport

import scevo.core.State
import scevo.core.StatePop

/** Evaluates population solution by solution.
 *  
 */
object SequentialEval {
  def apply[S, E](f: S => E) = (s: StatePop[S]) => StatePop(s.map(x => (x, f(x))))
}

/** Evaluates population in parallel. 
 *  The second version allows controlling the number of threads.
 *  
 */
object ParallelEval {
  def apply[S, E](f: S => E) =
    (s: StatePop[S]) => StatePop(s.par.map(x => (x, f(x))).to)

  def apply[S, E](f: S => E, parLevel: Int) = {
    val ts = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(parLevel))
    (s: StatePop[S]) => StatePop({
      val c = s.par
      c.tasksupport = ts
      c.map(x => (x, f(x))).to
    })
  }
}