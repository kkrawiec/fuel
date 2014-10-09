package scevo.examples

import org.junit.Test

import scevo.evo.BestHasProperty
import scevo.evo.EvaluatedSolution
import scevo.evo.Evolution
import scevo.evo.Experiment
import scevo.evo.GreedyBestSelection
import scevo.evo.IterativeAlgorithm
import scevo.evo.ScalarEvaluation
import scevo.evo.ScalarEvaluationMax
import scevo.evo.SearchStepWithEval
import scevo.evo.Selection
import scevo.evo.Selector
import scevo.evo.Solution
import scevo.evo.State
import scevo.evo.TournamentSelection
import scevo.tools.TRandom

class BitVector(val v: Seq[Boolean]) extends Solution {
  override val toString = v.map(if (_) "1" else "0").reduce(_ + _)
}

class BitVectorEvaluated(override val v: Seq[Boolean]) extends BitVector(v) with EvaluatedSolution[ScalarEvaluation] {

  override val eval = ScalarEvaluationMax(v.count(b => b)) //ensuring (_.v >= 0 && _.v <= v.size)

  def mutateOneBit(rng: TRandom) = {
    val bitToMutate = rng.nextInt(v.size)
    new BitVector(v.updated(bitToMutate, !v(bitToMutate)))
  } ensuring (_.v.size == v.size)

  def onePointCrossover(other: BitVectorEvaluated, rng: TRandom) = {
    require(other.v.size == v.size)
    val cuttingPoint = rng.nextInt(v.size)
    val (myHead, myTail) = v.splitAt(cuttingPoint)
    val (hisHead, hisTail) = other.v.splitAt(cuttingPoint)
    List(new BitVector(myHead ++ hisTail), new BitVector(hisHead ++ myTail))
  } ensuring (r => r(0).v.size == v.size && r(1).v.size == v.size)
}

abstract class ExperimentMaxOnes(args: Array[String]) extends Experiment[BitVectorEvaluated](args) {

  val numVars = options("numVars").toInt

  val evalFunc: BitVector => Option[BitVectorEvaluated] = bv => Some(new BitVectorEvaluated(bv.v))

  val initialState = State((0 until populationSize).map(_ => new BitVectorEvaluated(0 until numVars map (_ => rng.nextBoolean))))

  val searchOperators: Seq[(Selector[BitVectorEvaluated, ScalarEvaluation] => List[BitVector], Double)] = Seq(
    (source => List(source.next.mutateOneBit(rng)), operatorProbs(0)),
    (source => source.next.onePointCrossover(source.next, rng), operatorProbs(1)))

  def selection: Selection[BitVectorEvaluated, ScalarEvaluation]

  // Need laziness here as selection will be defined only *after* child class'es 
  // construction is completed. 
  lazy val searchAlg = new SearchStepWithEval[BitVector, BitVectorEvaluated, ScalarEvaluation](
    searchOperators, s => s.map(evalFunc(_)).flatten,
    selection, rng)

  val scIdealFitness = new BestHasProperty[BitVectorEvaluated]((s: BitVectorEvaluated) => s.eval.v == numVars)
  lazy val evol = new Evolution[BitVector, BitVectorEvaluated](initialState, searchAlg, List(scIdealFitness, scMaxGeneration, scMaxTime))

  override protected def run: Option[IterativeAlgorithm[BitVectorEvaluated]] = {
    evol.apply(super.postGenerationCallback)
    Some(evol)
  }
}

/* Genetic Algorithm
 */
object ExperimentMaxOnesGA {
  def main(args: Array[String]) = new ExperimentMaxOnes(args) {
    val tournamentSize = options("tournamentSize").toInt
    assert(tournamentSize > 1, "Tournament size should be > 1")
    override def selection = new TournamentSelection[BitVectorEvaluated, ScalarEvaluation](tournamentSize, rng)
  }.launch
}

/* Stochastic local search: 
 */
object ExperimentMaxOnesSLS {
  def main(args: Array[String]) = new ExperimentMaxOnes(args) {
    require(populationSize == 1)
    //  override def selection = new OnePlusOneSelection[BitVectorEvaluated]
    override def selection = new GreedyBestSelection[BitVectorEvaluated, ScalarEvaluation]
  }.launch
}

final class TestExperiment {
  // val params = "--seed 8 --populationSize 100 --maxTime 300000 --maxGenerations 1000 --operatorProbs 0.5,0.5 --numVars 200"
  val params = "--numVars 200  --tournamentSize 7  --operatorProbs 0.5,0.5"
  @Test
  def testExperimentMaxOnesGA =
    ExperimentMaxOnesGA.main(params.split("\\s+"))

  @Test
  def testExperimentMaxOnesSLS =
    ExperimentMaxOnesSLS.main((params + " --populationSize 1").split("\\s+"))
}