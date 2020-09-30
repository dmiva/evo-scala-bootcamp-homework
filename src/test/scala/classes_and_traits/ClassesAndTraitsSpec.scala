package classes_and_traits

import ClassesAndTraits._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ClassesAndTraitsSpec extends AnyFlatSpec with Matchers {

  "minimumBoundingRectangle" should "be correct" in {
    val mbr = Bounded.minimumBoundingRectangle(
      Set(
        Point(-12, -3),
        Point(-3, 7),
        Circle(Point(0,0), 5),
      )
    )

    mbr.minX shouldEqual -12
    mbr.maxX shouldEqual 5
    mbr.minY shouldEqual -5
    mbr.maxY shouldEqual 7
  }

  "circle moved by x=5" should "be correct" in {
    val circle = Circle(Point(3,5), 4)
    circle.move(5,0).x shouldEqual 8
  }
}
