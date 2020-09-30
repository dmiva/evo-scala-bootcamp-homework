package basics

import basics.ClassesAndTraits._
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

  "cube volume" should "be correct" in {
    val cube = Cube(Point3D(3,3,3), 3)
    cube.volume shouldEqual 3*3*3
  }

  "sphere volume" should "be correct" in {
    val sphere = Sphere(Point3D(3,3,3), 3.6278317)
    assert(sphere.volume - 200 < 0.00001)
  }

  "minimumBoundingBox" should "be correct" in {
    val mbb = Bounded3D.minimumBoundingBox(Set(
      Point3D(-12, -3, 4),
      Point3D(1, 7, -4.5),
      Sphere(Point3D(4,0,5), 5),
      Triangle3D(Point3D(2,2,4), 3,4,5)
    ))

    mbb.minX shouldEqual -12
    mbb.maxX shouldEqual 9
    mbb.minY shouldEqual -5
    mbb.maxY shouldEqual 7
    mbb.minZ shouldEqual -4.5
    mbb.maxZ shouldEqual 10
  }

}
