import FuzzySet.Universe
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class FuzzySetSpecification extends AnyFlatSpec with should.Matchers {

  implicit val fuzzySetUniverse: Universe[Int] = new Universe(Set.from(1 to 10))

  val emptyFuzzySet = new FuzzySet[Int](_ => 0.0)

  val someNonEmptyFuzzySet = new FuzzySet[Int]({
    case 1 => 0.5
    case 2 => 0.75
    case 3 => 1
    case _ => 0.0
  })

  val someNonEmptyFuzzySet1 = new FuzzySet[Int]({
    case 1 => 0.5
    case 2 => 0.75
    case 3 => 1
    case 4 => 0.25
    case _ => 0.0
  })

  val someNonEmptyFuzzySet2 = new FuzzySet[Int]({
    case 1 => 0.5
    case 2 => 0.75
    case 3 => 1
    case 4 => 0.25
    case _ => 0.0
  })

  val someNonEmptyFuzzySet3 = new FuzzySet[Int]({
    case 1 => 0.75
    case 2 => 0.6
    case 3 => 0.3
    case 4 => 0.4
    case _ => 0.0
  })

  "FuzzySet" should ("isEmpty") in {
    emptyFuzzySet.isEmpty should be(true)
    someNonEmptyFuzzySet.isEmpty should be(false)
    someNonEmptyFuzzySet1.isEmpty should be(false)
  }

  "FuzzySet.equalTo" should("equalTo") in {
    someNonEmptyFuzzySet.equalTo(emptyFuzzySet) should be (false)
    someNonEmptyFuzzySet.equalTo(someNonEmptyFuzzySet1) should be (false)
    someNonEmptyFuzzySet1.equalTo(someNonEmptyFuzzySet2) should be (true)
  }

  "FuzzySet.contains" should("contains") in {
    someNonEmptyFuzzySet.contains(1) should be (0.5)
    someNonEmptyFuzzySet.contains(4) should be (0.0)
    someNonEmptyFuzzySet1.contains(0) should be (0.0)
    emptyFuzzySet.contains(1) should be (0.0)
  }

  "FuzzySet.union" should("union") in {
    someNonEmptyFuzzySet.union(someNonEmptyFuzzySet3).contains(1) should be (0.75)
    someNonEmptyFuzzySet.union(someNonEmptyFuzzySet3).contains(4) should be (0.4)
    someNonEmptyFuzzySet.union(someNonEmptyFuzzySet3).contains(5) should be (0.0)
    someNonEmptyFuzzySet.union(someNonEmptyFuzzySet3).contains(20) should be (0.0)
  }

  "FuzzySet.intersect" should("intersect") in {
    someNonEmptyFuzzySet.intersect(someNonEmptyFuzzySet3).contains(1) should be (0.5)
    someNonEmptyFuzzySet.intersect(someNonEmptyFuzzySet3).contains(4) should be (0.0)
    someNonEmptyFuzzySet.intersect(someNonEmptyFuzzySet3).contains(5) should be (0.0)
    someNonEmptyFuzzySet.intersect(someNonEmptyFuzzySet3).contains(20) should be (0.0)
  }

  "FuzzySet.complement" should("complement") in {
    someNonEmptyFuzzySet.complement.contains(1) should be (0.5)
    someNonEmptyFuzzySet.complement.contains(4) should be (1)
    someNonEmptyFuzzySet.complement.contains(3) should be (0.0)
  }

}
/*// https://github.com/typelevel/scalacheck/blob/main/doc/UserGuide.md
object FuzzySetSpecification extends Properties("FuzzySet") {

  import ArbitraryFuzzySet._

  property("isEmpty") = forAll { a: ArbitraryFuzzySet[Int] =>
    !a.fuzzySet.isEmpty(a.universe)
  }

  property("equalTo") = forAll { a: ArbitraryFuzzySet[Int] =>
    a.fuzzySet.equalTo(a.fuzzySet)(a.universe)
  }

  property("contains") = forAll { (a: ArbitraryFuzzySet[Int] , x: Int) =>
    a.fuzzySet.contains(x) >= 0.0 && a.fuzzySet.contains(x) <= 1.0
  }

  property("intersect") = forAll { (a: ArbitraryFuzzySet[Int] , a1: ArbitraryFuzzySet[Int] , x: Int) =>
    val aResult = a.fuzzySet.intersect(a1.fuzzySet)
    aResult.contains(x) <= a.fuzzySet.contains(x) && aResult.contains(x) <= a1.fuzzySet.contains(x)
  }

  property("union") = forAll { (a: ArbitraryFuzzySet[Int] , a1: ArbitraryFuzzySet[Int] , x: Int) =>
    val aResult = a.fuzzySet.union(a1.fuzzySet)
    aResult.contains(x) >= a.fuzzySet.contains(x) && aResult.contains(x) >= a1.fuzzySet.contains(x)
  }

  property("complement") = forAll { (a: ArbitraryFuzzySet[Int] , x: Int) =>
    val aResult = a.fuzzySet.complement(a.universe)
    aResult.contains(x) + a.fuzzySet.contains(x) == 1
  }


}

case class ArbitraryFuzzySet[T](fuzzySet: FuzzySet[T], universe: Universe[T])

object ArbitraryFuzzySet {

  implicit def arbitraryNonEmptyUniverse[T](implicit a: Arbitrary[T]): Arbitrary[Universe[T]] =
    Arbitrary {
      for {
        values <- Gen.nonEmptyContainerOf[Set, T](a.arbitrary)
      } yield new Universe(values)
    }

  implicit def arbitraryNonEmptyFuzzySet[T](implicit a: Arbitrary[Universe[T]]): Arbitrary[ArbitraryFuzzySet[T]] =
    Arbitrary {
      for {
        universe <- a.arbitrary
        values <- Gen.nonEmptyContainerOf[List, T](Gen.oneOf(universe.values))
        grades <- Gen.containerOfN[List, Double](values.size, Gen.choose(0.0, 1.0) suchThat (v => v != 0.0))
      } yield {
        val fuzzySet = new FuzzySet[T]({ v: T =>
          val index = values.indexOf(v)
          if (index < 0) 0.0 else grades(index)
        })

        ArbitraryFuzzySet(fuzzySet, universe)
      }
    }
}*/
