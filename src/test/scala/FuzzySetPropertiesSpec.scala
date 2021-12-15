import FuzzySet.Universe
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Gen, Properties}


object FuzzySetPropertiesSpec extends Properties("FuzzySet") {

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
}

