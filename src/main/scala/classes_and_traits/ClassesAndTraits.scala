package classes_and_traits

object ClassesAndTraits {

  // Homework
  //
  // Add additional 2D shapes such as triangle and square.
  //
  // In addition to the 2D shapes classes, add also 3D shapes classes
  // (origin, point, sphere, cube, cuboid, 3D triangle - you can add
  // others if you think they are a good fit).
  //
  // Add method `area` to 2D shapes.
  //
  // Add methods `surfaceArea` and `volume` to 3D shapes.
  //
  // If some of the implementation involves advanced math, it is OK
  // to skip it (leave unimplemented), the primary intent of this
  // exercise is modelling using case classes and traits, and not math.

  // Given data
  sealed trait Shape[A] extends Located with Bounded with Movable[A]

  sealed trait Located {
    def x: Double
    def y: Double
  }

  sealed trait Bounded {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
  }

  sealed trait Movable[A] {
    def move(dx: Double, dy: Double): A
  }

  object Origin extends Located {
    override def x: Double = 0
    override def y: Double = 0
  }

  final case class Point(x: Double, y: Double) extends Shape[Point] {
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y
    override def move(dx: Double, dy: Double): Point = Point(x + dx, y + dy)
  }

  final case class Circle(centerX: Double, centerY: Double, radius: Double) extends Shape[Circle] {
    override def x: Double = centerX
    override def y: Double = centerY
    override def minX: Double = centerX - radius
    override def maxX: Double = centerX + radius
    override def minY: Double = centerY - radius
    override def maxY: Double = centerY + radius
    override def move(dx: Double, dy: Double): Circle = Circle(centerX + dx, centerY + dy, radius)
  }

  final case class Rectangle(centerX: Double, centerY: Double, length: Double, width: Double) extends Shape[Rectangle] {
    override def x: Double = centerX
    override def y: Double = centerY
    override def minX: Double = centerX - length/2
    override def maxX: Double = centerX + length/2
    override def minY: Double = centerY - width/2
    override def maxY: Double = centerY + width/2
    override def move(dx: Double, dy: Double): Rectangle = Rectangle(centerX + dx, centerY + dy, length, width)
  }

  final case class Square(centerX: Double, centerY: Double, length: Double) extends Shape[Square] {
    override def x: Double = centerX
    override def y: Double = centerY
    override def minX: Double = centerX - length/2
    override def maxX: Double = centerX + length/2
    override def minY: Double = centerY - length/2
    override def maxY: Double = centerY + length/2
    override def move(dx: Double, dy: Double): Square = Square(centerX + dx, centerY + dy, length)
  }

  // Currently not fully generic triangle
  final case class Triangle(centerX: Double, bottomY: Double, base: Double, height: Double) extends Shape[Triangle] {
    override def x: Double = centerX
    override def y: Double = bottomY
    override def minX: Double = centerX - base/2
    override def maxX: Double = centerX + base/2
    override def minY: Double = bottomY - height
    override def maxY: Double = bottomY
    override def move(dx: Double, dy: Double): Triangle = Triangle(centerX + dx, bottomY + dy, base, height)
  }


}
