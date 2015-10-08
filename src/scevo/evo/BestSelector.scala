package scevo.evo

object BestSelector {

  def apply[S, E](set: Seq[(S, E)], o: PartialOrdering[E]) =
    set.reduceLeft((a, b) => if (o.tryCompare(a._2, b._2).getOrElse(0) < 0) a else b)

 def apply[S, E](set: Seq[(S, E)], better: (E, E) => Boolean) =
    set.reduceLeft((a, b) => if (better(a._2, b._2)) a else b)

  def apply[T](set: Seq[T], compare: (T, T) => Int) =
    set.reduceLeft((a, b) => if (compare(a, b) < 0) a else b)

  def apply[T](set: Seq[T], ord: Ordering[T]) =
    set.reduceLeft((a, b) => if (ord.compare(a, b) < 0) a else b)
}

object WorstSelector {
  def apply[T](set: Seq[T], ord: Ordering[T]) =
    set.reduceLeft((a, b) => if (ord.compare(a, b) > 0) a else b)
}

