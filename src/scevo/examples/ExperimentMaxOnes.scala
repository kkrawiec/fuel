package scevo.examples

import scevo.evo.Solution
import scevo.evo.Experiment
import scevo.evo.SearchAlgorithmWithEval
import scevo.evo.Evolution
import scevo.evo.TournamentSelection
import scevo.tools.TRandom
import scevo.evo.EvaluatedSolution
import scevo.evo.State
import scevo.evo.GreedyBestSelection
import scevo.evo.Selection
import scevo.evo.OnePlusOneSelection


class BitVector( val v: Seq[Boolean] ) extends Solution {
  def this(numVars: Integer, r: TRandom) =
    this(v = 0 until numVars map (i => r.nextBoolean))
  override def toString() = v.map( if(_) "1" else "0").reduce( (b1,b2) => b1+b2 )
}

class BitVectorEvaluated( val v: Seq[Boolean] ) extends EvaluatedSolution {

  override def fitness = v.count(b => b) ensuring( r => r >=0 && r <= v.size )
  override def betterThan(other: EvaluatedSolution): Option[Boolean] = Some(fitness > other.fitness)
//  override def toString() = v.toString
  override def toString() = v.map( if(_) "1" else "0").reduce( (b1,b2) => b1+b2 )

  def mutateOneBit(rng: TRandom) = {
    val bitToMutate = rng.nextInt(v.size)
    new BitVector(0 until v.size map (i => if (i == bitToMutate) !v(i) else v(i)))
  } ensuring( _.v.size == v.size )

  def onePointCrossover( other: BitVectorEvaluated, rng: TRandom) : (BitVector, BitVector) = {
    require( other.v.size == v.size )
    val cuttingPoint = rng.nextInt(v.size)
    val me = v.splitAt(cuttingPoint)
    val oth = other.v.splitAt(cuttingPoint)
    (new BitVector(me._1 ++ oth._2), new BitVector(me._2 ++ oth._1)) 
  } ensuring( r => r._1.v.size == v.size && r._2.v.size == v.size )
}

abstract class ExperimentMaxOnes(args: Array[String]) extends Experiment(args) {

  val numVars = options("numVars").toInt

  val initialState = State.apply(populationSize, new BitVector(numVars, rng))

  val searchOperators: Seq[(BitVectorEvaluated => BitVector, Double)] = IndexedSeq(
    (es => es.mutateOneBit(rng), operatorProbs(0)))

  val evalFunc: BitVector => Option[BitVectorEvaluated] = bv => Some(new BitVectorEvaluated(bv.v))

  val evol = new Evolution[BitVector, BitVectorEvaluated](initialState)
  
 val searchAlg = new SearchAlgorithmWithEval[BitVector, BitVectorEvaluated](
    searchOperators, evalFunc, selection, rng)

  def selection : Selection[BitVectorEvaluated] 

  override protected def run: (Evolution[_, _], State[_], EvaluatedSolution) = {
    val res = evol.apply(searchAlg, List(scMaxGeneration, scMaxTime), None, None)
    (evol, res._1, res._2)
  }

}

/* Genetic Algorithm
 */
class ExperimentMaxOnesGA(args: Array[String]) extends ExperimentMaxOnes(args) {
  override def selection = new TournamentSelection[BitVectorEvaluated](tournamentSize, rng)
}

object ExperimentMaxOnesGA {
  def main(args: Array[String]) = new ExperimentMaxOnesGA(args).launch
}


/* Stochastic local search: 
 */
class ExperimentMaxOnesSLS(args: Array[String]) extends ExperimentMaxOnes(args) {
  require( populationSize == 1 )
//  override def selection = new OnePlusOneSelection[BitVectorEvaluated]
  override def selection = new GreedyBestSelection[BitVectorEvaluated]
}

object ExperimentMaxOnesSLS {
  def main(args: Array[String]) = new ExperimentMaxOnesSLS(args).launch
}
