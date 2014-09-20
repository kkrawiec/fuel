package scevo.evo


class NSGASelection[ES <: EvaluatedSolution[F], F <: MultiobjectiveEvaluation](
  val numToGenerate: Int)
  extends Selection[ES, F] {

  // TODO: treat indiscernibility and incomparability in the same way
  def paretoRanking(solutions: Seq[ES]): (List[Set[ES]], Array[Array[Option[Int]]]) = {
    val n = solutions.length
    var cmp = Array.ofDim[Option[Int]](n, n)
    for (i <- 0 until n) {
      for (j <- 0 until i) {
        val v = solutions(i).eval.comparePartial(solutions(j).eval)
        cmp(i)(j) = v
        cmp(j)(i) = if (v.isEmpty) None else Some(-v.get)
      }
      cmp(i)(i) = Some(0)
    }
    import scala.collection.mutable.MutableList
    var layers = MutableList[Set[ES]]()
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
    (layers.toList, cmp)
  }

  override def selector(history: Seq[State[ES]]) = new Selector[ES, F] {

    val pool = history.head.solutions
    val previous = if (history.size < 2) List() else history.tail.head.solutions

    val selected = {
      val (ranking, dominanceMatrix) = paretoRanking((pool ++ previous)) //.asInstanceOf[Seq[MultiobjectiveFitness]])
      var capacity = numToGenerate
      val fullLayers = ranking.takeWhile(r => if (capacity < numToGenerate) false else { capacity -= r.size; true })
      val partialLayer = fullLayers.length

      (fullLayers.flatten ++ { // flatten preserves ordering
        if (partialLayer == ranking.length || capacity <= 0)
          None
        else { // the most sparse individuals from the partial rank
          val sparsity = ranking(partialLayer).groupBy(s => s.eval.v).map(p => (p._1, p._2.size))
          ranking(partialLayer).toSeq.sortBy(s => sparsity(s.eval.v)).splitAt(capacity)._1
        }
      })
    }

    private var iter: Int = -1
    override def next: ES = {
      iter = (iter + 1) % selected.size
      selected(iter)
    }
    override val numSelected = pool.size

  }
}

/*
trait Dominance[T <: MultiobjectiveFitness] 
{
  def apply(x: T, y: T): Option[Int]
}

class DominanceMinimized[T <: MultiobjectiveFitness] extends Dominance[T] {
  override def apply(x: T, y: T): Option[Int] = {
    val n = x.v.length
    assert(n == y.v.length)
    var xBetter: Int = 0
    var yBetter: Int = 0
    for (i <- 0 until n)
      if (x.v(i).better(y.v(i)))
        xBetter += 1
      else if (y.v(i).better(x.v(i)))
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
* 
*/
