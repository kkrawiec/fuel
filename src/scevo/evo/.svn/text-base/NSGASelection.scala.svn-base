package scevo.evo

import scala.collection.mutable.MutableList

trait MultiobjectiveFitness extends Fitness {
  def objValues: Array[Double]
}

trait Dominance[+T] // extends ((T, T) => Option[Int])
{
  def apply[ S >: T <: MultiobjectiveFitness ](x: S, y: S): Option[Int] 
}


class DominanceMinimized[+T <: MultiobjectiveFitness] extends Dominance [T] {
  override def apply[ S >: T <: MultiobjectiveFitness ](x: S, y: S): Option[Int] = {
    val n = x.objValues.length
    assert(n == y.objValues.length)
    var xBetter: Int = 0
    var yBetter: Int = 0
    for (i <- 0 until n)
      if (x.objValues(i) < y.objValues(i))
        xBetter += 1
      else if (x.objValues(i) > y.objValues(i))
        yBetter += 1

    if (xBetter > 0)
      if (yBetter > 0)
        None
      else
        Some(1)
    else if (yBetter > 0)
      Some(-1)
    else
      Some(0)
  }
}


class NSGASelection[ES <: EvaluatedSolution with MultiobjectiveFitness](val comparator: Dominance[ES])
  extends Selection [ES] {

  // TODO: treat indiscernibility and incomparability in the same way
  def paretoRanking[ T >: ES <: MultiobjectiveFitness ](solutions: Seq[T]): (MutableList[Set[T]], Array[Array[Option[Int]]]) = {
    val n = solutions.length
    var cmp = Array.ofDim[Option[Int]](n, n)
    for (i <- 0 until n) {
      for (j <- 0 until i) {
        val v = comparator(solutions(i), solutions(j))
        cmp(i)(j) = v
        cmp(j)(i) = if (v.isEmpty) None else Some(-v.get)
      }
      cmp(i)(i) = Some(0)
    }
    var layers = MutableList[Set[T]]()
    var remaining = (0 until n).toSet
    while (!remaining.isEmpty) {
//      println( "aa: " + remaining.size )
      val dominated = remaining.filter(i =>
        remaining.find(j => cmp(i)(j).getOrElse(1) < 0).isDefined)
      val rank = if (dominated.isEmpty) remaining else remaining -- dominated
      layers = (layers.:+(rank.map(e => solutions(e))))
      remaining = remaining -- rank
    }
    //val sparsity = cmp.map( row => 1.0 / row.count( e => e.contains(0)))
//    println( "Total: " + layers.map( _.size ).sum ) 
    (layers, cmp)
  }

  override def apply[ T >: ES ](pool: Seq[T], numToGenerate: Int, previous: Seq[T] ): Seq[T] =
//  def apply(current: Seq[T], size: Int, previous: Seq[T]): Seq[T] = {
  {
    // TODO: Cannot require T to be subtype of MultiobjectiveFitness, because then this apply() method
    // does not conform the parent class definition; hence this nasty trick:
    val (ranking, dominanceMatrix) = paretoRanking((pool ++ previous).asInstanceOf[Seq[MultiobjectiveFitness]])
    var capacity = numToGenerate
    val fullLayers = ranking.takeWhile(r => if (capacity < numToGenerate) false else { capacity -= r.size; true })
    val partialLayer = fullLayers.length

    (fullLayers.flatten ++ { // flatten preserves ordering
      if (partialLayer == ranking.length || capacity <= 0)
        None
      else { // the most sparse individuals from the partial rank
        val sparsity = ranking(partialLayer).groupBy(s => s.objValues).map(p => (p._1, p._2.size))
        ranking(partialLayer).toSeq.sortBy(s => sparsity(s.objValues)).splitAt(capacity)._1
      }
    }).asInstanceOf[Seq[T]] // similar explanation as above
  }
}
