import scala.util.Try

object FuzzySet {
  class Universe[T](val values: Set[T])
}

class FuzzySet[T](m: T => Double) {
  import FuzzySet.Universe

  def isEmpty(implicit universe: Universe[T]): Boolean =
    universe.values.forall(m(_) == 0.0)

  def equalTo(that: FuzzySet[T])(implicit universe: Universe[T]): Boolean =
    universe.values.forall(x => m(x) == that.contains(x))

  def contains(value: T): Double = Try {m(value)}.toOption.getOrElse(0.0)

  def union(that: FuzzySet[T]): FuzzySet[T] = {
    def m2 (x: T ) = if ( m(x) >= that.contains(x)) m(x) else that.contains(x)
    new FuzzySet(m2)
  }

  def intersect(that: FuzzySet[T]): FuzzySet[T] = {
    def m2 (x: T) = if (m(x) <= that.contains(x)) m(x) else that.contains(x)
    new FuzzySet(m2)
  }

  def complement(implicit universe: Universe[T]): FuzzySet[T] = {
    new FuzzySet[T](1 - m(_))
  }

}

object FuzzySetApp extends App {
  import FuzzySet.Universe

  implicit val fuzzySetUniverse: Universe[Int] = new Universe(Set.from(1 to 10))

  val emptyFuzzySet = new FuzzySet[Int](_ => 0.0)
  println(emptyFuzzySet.isEmpty)

  val someNonEmptyFuzzySet = new FuzzySet[Int]({
    case 1 => 0.5
    case 2 => 0.75
    case 3 => 1
    case _ => 0.0
  })

  println(someNonEmptyFuzzySet.isEmpty)

  val someNonEmptyFuzzySet1 = new FuzzySet[Int]({
    case 1 => 0.5
    case 2 => 0.75
    case 3 => 1
    case _ => 0.0
  })

  val someNonEmptyFuzzySet2 = new FuzzySet[Int]({
    case 1 => 0.5
    case 2 => 0.75
    case 3 => 1
    case 4 => 0.25
    case _ => 0.0
  })

  println(someNonEmptyFuzzySet.equalTo(emptyFuzzySet))

  //println(someNonEmptyFuzzySet.equalTo(someNonEmptyFuzzySet1))

  //println(someNonEmptyFuzzySet2.union(someNonEmptyFuzzySet1))

  //println(someNonEmptyFuzzySet2.intersect(someNonEmptyFuzzySet1))

  //println(someNonEmptyFuzzySet1.contains(-234677895))
}