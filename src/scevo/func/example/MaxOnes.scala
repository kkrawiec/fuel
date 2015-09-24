package scevo.func.example

import scala.collection.immutable.BitSet
import scevo.func.Breeder
import scevo.func.EnvAndRng
import scevo.func.EnvFromArgs
import scevo.func.Environment
import scevo.func.Experiment
import scevo.func.IndependentEval
import scevo.func.IterativeAlgorithm
import scevo.func.ParallelEval
import scevo.func.RandomMultiBreeder
import scevo.func.RandomStatePop
import scevo.func.StatePop
import scevo.func.Termination
import scevo.func.TournamentSelection
import scevo.tools.Rng
import scevo.tools.TRandom

/**
  * Use case: MaxOnes with GA. 
  *
  */

/**
  * Style 0: Compact, fitness defined as a member function, parallel evaluation,
  * solutions represented as BitSets (TreeSet much slower)
  *
  */

object GA0 {

  // Candidate solution (bitstring)
  class S(val v: BitSet) {
    override val toString = v.toString
    def fitness = v.size
  }

  def apply(env: Environment): Unit = {
    val rng = Rng(env)
    val numVars = env.paramInt("numVars", _ > 0)
    def initializer = RandomStatePop(env,
      () => new S(BitSet.empty ++
        (for (i <- 0.until(numVars); if (rng.nextBoolean)) yield i)))

    def eval = ParallelEval((s: S) => s.fitness)
    def iteration = eval compose Breeder[S, Int](
      TournamentSelection(env)(rng)(Ordering[Int]),
      RandomMultiBreeder(rng, env)(Seq( // Each search operator: Stream[S] => (List[S], Stream[S])
        (source: Stream[S]) => { // One-bit mutation:
          val s = source.head
          val bitToMutate = rng.nextInt(numVars)
          (List(new S(if (s.v(bitToMutate)) s.v - bitToMutate else s.v + bitToMutate)), source.tail)
        },
        (source: Stream[S]) => { // One-point crossover: 
          val cuttingPoint = rng.nextInt(numVars)
          val (myHead, myTail) = source(0).v.partition(_ < cuttingPoint)
          val (hisHead, hisTail) = source(1).v.partition(_ < cuttingPoint)
          (List(new S(myHead ++ hisTail), new S(hisHead ++ myTail)), source.drop(2))
        })))

    def stopBestFit = (s: StatePop[(S, Int)]) => s.solutions.exists(_._2 == numVars)

    // Compose the components into a search algorithm:
    def alg = initializer andThen eval andThen
      IterativeAlgorithm(env)(iteration)(Termination[S, Int](env) :+ stopBestFit)(Ordering[Int])

    // Run the algorithm:
    Experiment(env)(alg)()
  }
}

object TestGA0 {
  def main(args: Array[String]) {
    GA0(EnvFromArgs("--numVars 500  --maxGenerations 1000 --populationSize 1000 "))
  }
}

/*

/* Style 1: Compact, fitness defined as a member function, parallel evaluation,
 * fast toString (the other one is really slow)
 * 
 */
object GA1 {

  // Candidate solution (bitstring)
  class S(val v: Seq[Boolean]) {
    override val toString = {
      val sb = new StringBuilder()
      v.foreach(e => sb.append(if (e) "1" else "0"))
      sb.toString
    }
    def fitness = ScalarEvaluationMax(v.count(e => e))
  }

  def apply(env: Environment): Unit = {
    val rng = Rng(env)
    val numVars = env.paramInt("numVars", _ > 0)
    def initializer = RandomStatePop(env, () => new S(IndexedSeq.fill(numVars)(rng.nextBoolean)))
    // List much less effective
    // Int less effective than Boolean

    type E = ScalarEvaluationMax
    def eval = ParallelEval((s: S) => s.fitness)
    def iteration = eval compose Breeder[S, E](
      TournamentSelection[S, E](env)(rng),
      RandomMultiBreeder(rng, env)(Seq( // Each search operator: Stream[S] => (List[S], Stream[S])
        (source: Stream[S]) => { // One-bit mutation:
          val s = source.head
          val bitToMutate = rng.nextInt(s.v.size)
          (List(new S(s.v.updated(bitToMutate, !s.v(bitToMutate)))), source.tail)
        },
        (source: Stream[S]) => { // One-point crossover: 
          val me = source(0)
          val cuttingPoint = rng.nextInt(me.v.size)
          val (myHead, myTail) = me.v.splitAt(cuttingPoint)
          val (hisHead, hisTail) = source(1).v.splitAt(cuttingPoint)
          (List(new S(myHead ++ hisTail), new S(hisHead ++ myTail)), source.drop(2))
        })))

    def stopBestFit = (s: StatePop[(S, E)]) => s.solutions.exists(_._2.v == numVars)

    // Compose the components into a search algorithm:
    def alg = initializer andThen eval andThen
      IterativeAlgorithm(env)(iteration)(Termination[S, E](env) :+ stopBestFit)

    // Run the algorithm:
    Experiment(env)(alg)()
  }
}

object TestGA1 {
  def main(args: Array[String]) {
    GA1(EnvFromArgs("--numVars 500  --maxGenerations 1000 --populationSize 1000 "))
  }
}

/* Style 2: Fitness as an independent function (evaluate). 
 * 
 */

object GA2 {

  // Candidate solution (bitstring)
  class S(val v: Vector[Boolean]) {
    override val toString = v.map(if (_) "1" else "0").reduce(_ + _)
  }

  // Shorthands: 
  type E = ScalarEvaluationMax
  type ES = (S, E) // Evaluated solution

  // Fitness function = the number of ones:
  def evaluate(p: S) = ScalarEvaluationMax(p.v.count(b => b))

  def main(args: Array[String]): Unit = {
    val (env, rng) = EnvAndRng(args)
    val numVars = env.paramInt("numVars", _ > 0)
    def initializer = RandomStatePop(env, () => new S(Vector.fill(numVars)(rng.nextBoolean)))

    // A search operator: Stream[S] => (List[S], Stream[S])
    def operators(rng: TRandom) = Seq(
      (source: Stream[S]) => { // One-bit mutation:
        val s = source.head
        val bitToMutate = rng.nextInt(s.v.size)
        (List(new S(s.v.updated(bitToMutate, !s.v(bitToMutate)))), source.tail)
      },
      (source: Stream[S]) => { // One-point crossover: 
        val me = source(0)
        val cuttingPoint = rng.nextInt(me.v.size)
        val (myHead, myTail) = me.v.splitAt(cuttingPoint)
        val (hisHead, hisTail) = source(1).v.splitAt(cuttingPoint)
        (List(new S(myHead ++ hisTail), new S(hisHead ++ myTail)), source.drop(2))
      })

    // Remaining components: 
    def sel = TournamentSelection[S, E](env)(rng)
    def eval = IndependentEval(evaluate)
    def rmp = RandomMultiBreeder(rng, env)(operators(rng))
    def iteration = Breeder[S, E](sel, rmp) andThen eval
    def stopBestFit = (s: StatePop[ES]) => s.solutions.exists(_._2.v == numVars)

    // Compose the components into search algorithm:
    def alg = initializer andThen eval andThen
      IterativeAlgorithm(env)(iteration)(Termination[S, E](env) :+ stopBestFit)

    // Run the algorithm:
    Experiment(env)(alg)()
  }
}

object TestGA2 {
  def main(args: Array[String]) {
    TestGA2.main(Array("--numVars", "50", "--maxGenerations", "100"))
  }
}
/*
    val config = new OptionsFromArgs(args) // TODO: Detach options from collector
    //    def alg = IterativeAlgorithm[S,E](env)(initializer andThen IndependentEvaluation(evaluate))(iteration)(Termination[S, E](config) :+ stopMaxFit)
    //    def alg = IterativeAlgorithm[StatePop[ES]](initializer andThen eval)(iteration)(Termination[S, E](config) :+ stopMaxFit)(EpilogueBestOfRun[S, E](bsf, config))
trait Solver[Sol <: Solution] extends (() => Sol)

*/

*/