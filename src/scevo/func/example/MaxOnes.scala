package scevo.func.example

import scevo.func.Experiment
import scevo.tools.OptAndColl
import scevo.tools.Rng
import scevo.func.BitSetDomain
import scevo.func.SimpleGA
import scala.collection.immutable.BitSet

/**
  * Use case: MaxOnes with GA.
  *
  */
object TestGA0 {
  def main(args: Array[String]) {
    implicit val (opt, coll) = OptAndColl("--numVars 500  --maxGenerations 1000 --populationSize 1000 ")
    implicit val rng = Rng(opt)

    val ga = new SimpleGA[BitSet, Int](
      domain = BitSetDomain(opt.paramInt("numVars", _ > 0)),
      eval = (s: BitSet) => s.size,
      stop = (s: BitSet, e: Int) => e == 0)

    // Create the experiment and launch it:
    val exp = Experiment(ga)
    exp()
  }
}

object TestTSP {
  def main(args: Array[String]) {
    implicit val (opt, coll) = OptAndColl("--numCities 10  --maxGenerations 1000 --populationSize 1000 ")
    implicit val rng = Rng(opt)

    val numCities = opt.paramInt("numVars", _ > 0)
    val size = 10.0
    val cities = 0.until(numCities).map(i => (size * rng.nextDouble, size * rng.nextDouble))
    def dist(i: Int, j: Int) = math.sqrt(
      math.pow(cities(i)._1 - cities(j)._1, 2) + math.pow(cities(i)._2 - cities(j)._2, 2))
    val distances =
      for (i <- 0 until numCities) yield for (j <- 0 until numCities) yield dist(i, j)

    val ga = new SimpleGA[Seq[Int], Double](
      new TSPDomain(numCities)(rng),
      (s: Seq[Int]) => 0.until(s.size-1).map(i => dist(s(i),s(i+1))).sum + dist(s.size-1,0)
      )

    // Create the experiment and launch it:
    val exp = Experiment(ga)
    exp()
  }
}/*

/* Style 1: Candidate solution as a separate class, fitness defined as a member function, parallel evaluation,
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
    def fitness = v.count(_ == true)
  }

  def apply(env: Environment): Unit = {
    val rng = Rng(env)
    val numVars = env.paramInt("numVars", _ > 0)
    def initializer = RandomStatePop(env, () => new S(IndexedSeq.fill(numVars)(rng.nextBoolean)))
    // List much less effective
    // Int less effective than Boolean

    def eval = ParallelEval((s: S) => s.fitness)
    def iteration = eval compose SimpleBreeder[S, Int](
      TournamentSelection(env)(rng)(Ordering[Int]),
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

    def stopBestFit = (s: StatePop[(S, Int)]) => s.solutions.exists(_._2 == 0)

    // Compose the components into a search algorithm:
    def alg = initializer andThen eval andThen
      IterativeAlgorithm[S, Int](env)(iteration)(Termination[S, Int](env) :+ stopBestFit)(Ordering[Int])

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