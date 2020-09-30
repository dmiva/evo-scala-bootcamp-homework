package basics

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

  sealed trait Shape[A] extends Located with Bounded with Movable[A] with Spaced

  sealed trait Shape3D[A] extends Located3D with Bounded3D with Movable3D[A] with Volumetric

  sealed trait Located {
    def x: Double
    def y: Double
  }

  sealed trait Located3D extends Located {
    def z: Double
  }

  sealed trait Bounded {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
  }

  sealed trait Bounded3D extends Bounded {
    def minZ: Double
    def maxZ: Double
  }

  object Bounded {
    def minimumBoundingRectangle(objects: Set[Bounded]): Bounded = {
      new Bounded {
        implicit private val doubleOrdering: Ordering[Double] = Ordering.Double.IeeeOrdering

        override def minX: Double = objects.map(_.minX).min
        override def maxX: Double = objects.map(_.maxX).max
        override def minY: Double = objects.map(_.minY).min
        override def maxY: Double = objects.map(_.maxY).max
      }
    }
  }

  object Bounded3D {
    def minimumBoundingBox(objects: Set[Bounded3D]): Bounded3D = {
      new Bounded3D {
        implicit private val doubleOrdering: Ordering[Double] = Ordering.Double.IeeeOrdering

        override def minX: Double = objects.map(_.minX).min
        override def maxX: Double = objects.map(_.maxX).max
        override def minY: Double = objects.map(_.minY).min
        override def maxY: Double = objects.map(_.maxY).max
        override def minZ: Double = objects.map(_.minZ).min
        override def maxZ: Double = objects.map(_.maxZ).max
      }
    }
  }

  sealed trait Movable[A] {
    def move(dx: Double, dy: Double): A
  }

  sealed trait Movable3D[A] {
    def move(dx: Double, dy: Double, dz: Double): A
  }

  sealed trait Spaced {
    def area: Double
  }

  sealed trait Volumetric {
    def surfaceArea: Double
    def volume: Double
  }

  object Origin extends Located {
    override def x: Double = 0
    override def y: Double = 0
  }

  object Origin3D extends Located3D {
    override def x: Double = 0
    override def y: Double = 0
    override def z: Double = 0
  }

  // 2D shapes
  final case class Point(x: Double, y: Double) extends Shape[Point] {
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y
    override def move(dx: Double, dy: Double): Point = Point(x + dx, y + dy)
    override def area: Double = 0
  }

  final case class Circle(origin: Point, radius: Double) extends Shape[Circle] {
    override def x: Double = origin.x
    override def y: Double = origin.y
    override def minX: Double = origin.x - radius
    override def maxX: Double = origin.x + radius
    override def minY: Double = origin.y - radius
    override def maxY: Double = origin.y + radius
    override def move(dx: Double, dy: Double): Circle = Circle(origin.move(dx, dy), radius)
    override def area: Double = Math.PI * radius * radius
  }

  final case class Rectangle(origin: Point, length: Double, width: Double) extends Shape[Rectangle] {
    override def x: Double = origin.x
    override def y: Double = origin.y
    override def minX: Double = origin.x - length/2
    override def maxX: Double = origin.x + length/2
    override def minY: Double = origin.y - width/2
    override def maxY: Double = origin.y + width/2
    override def move(dx: Double, dy: Double): Rectangle = Rectangle(origin.move(dx, dy), length, width)
    override def area: Double = length * width
  }

  final case class Square(origin: Point, length: Double) extends Shape[Square] {
    override def x: Double = origin.x
    override def y: Double = origin.y
    override def minX: Double = origin.x - length/2
    override def maxX: Double = origin.x + length/2
    override def minY: Double = origin.y - length/2
    override def maxY: Double = origin.y + length/2
    override def move(dx: Double, dy: Double): Square = Square(origin.move(dx, dy), length)
    override def area: Double = length * length
  }

  // Right-angled triangle
  final case class Triangle(origin: Point, base: Double, height: Double) extends Shape[Triangle] {
    override def x: Double = origin.x // bottom X
    override def y: Double = origin.y // left Y
    override def minX: Double = origin.x
    override def maxX: Double = origin.x + base
    override def minY: Double = origin.y
    override def maxY: Double = origin.y + height
    override def move(dx: Double, dy: Double): Triangle = Triangle(origin.move(dx, dy), base, height)
    override def area: Double = (base + height) / 2
  }

  def describe[A](x: Shape[A]): String = x match {
    case Point(x, y) => s"Point(x = $x, y = $y)"
    case Circle(origin, radius) => s"Circle(Point(x = ${origin.x}, y = ${origin.y}), radius = $radius)"
    case Rectangle(origin, length, width) => s"Rectangle(Point(x = ${origin.x}, y = ${origin.y}), length = $length, width = $width)"
    case Square(origin, length) => s"Square(Point(x = ${origin.x}, y = ${origin.y}), length = $length)"
    case Triangle(origin, base, height) => s"Triangle(Point(x = ${origin.x}, y = ${origin.y}), base = $base, height = $height)"
  }

  // 3D shapes
  final case class Point3D(x: Double, y: Double, z: Double) extends Shape3D[Point3D] {
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y
    override def minZ: Double = z
    override def maxZ: Double = z
    override def move(dx: Double, dy: Double, dz: Double): Point3D = Point3D(x + dx, y + dy, z + dz)
    override def surfaceArea: Double = 0
    override def volume: Double = 0
    //override def toString: String = s"Point3D(x = $x, y = $y, z = $z)"
  }

  final case class Sphere(origin: Point3D, radius: Double) extends Shape3D[Sphere] {
    override def x: Double = origin.x
    override def y: Double = origin.y
    override def z: Double = origin.z
    override def minX: Double = origin.x - radius
    override def maxX: Double = origin.x + radius
    override def minY: Double = origin.y - radius
    override def maxY: Double = origin.y + radius
    override def minZ: Double = origin.z - radius
    override def maxZ: Double = origin.z + radius
    override def move(dx: Double, dy: Double, dz: Double): Sphere = Sphere(origin.move(dx, dy, dz), radius)
    override def surfaceArea: Double = 4 * Math.PI * radius * radius
    override def volume: Double = (4/3.0) * Math.PI * Math.pow(radius, 3)
  }

  final case class Cuboid(origin: Point3D, length: Double, width: Double, height: Double) extends Shape3D[Cuboid] {
    override def x: Double = origin.x
    override def y: Double = origin.y
    override def z: Double = origin.z
    override def minX: Double = origin.x - length
    override def maxX: Double = origin.x + length
    override def minY: Double = origin.y - width
    override def maxY: Double = origin.y + width
    override def minZ: Double = origin.z - height
    override def maxZ: Double = origin.z + height
    override def move(dx: Double, dy: Double, dz: Double): Cuboid = Cuboid(origin.move(dx, dy, dz), length, width, height)
    override def surfaceArea: Double = 2 * (length*width + length*height + width*height)
    override def volume: Double = length * width * height
  }

  final case class Cube(origin: Point3D, length: Double) extends Shape3D[Cube] {
    override def x: Double = origin.x
    override def y: Double = origin.y
    override def z: Double = origin.z
    override def minX: Double = origin.x - length
    override def maxX: Double = origin.x + length
    override def minY: Double = origin.y - length
    override def maxY: Double = origin.y + length
    override def minZ: Double = origin.z - length
    override def maxZ: Double = origin.z + length
    override def move(dx: Double, dy: Double, dz: Double): Cube = Cube(origin.move(dx, dy, dz), length)
    override def surfaceArea: Double = 6 * length * length
    override def volume: Double = Math.pow(length, 3)
  }

  // Right triangular pyramid
  final case class Triangle3D(origin: Point3D, base: Double, baseHeight: Double, height: Double) extends Shape3D[Triangle3D] {
    override def x: Double = origin.x
    override def y: Double = origin.y
    override def z: Double = origin.z
    override def minX: Double = origin.x
    override def maxX: Double = origin.x + base
    override def minY: Double = origin.y
    override def maxY: Double = origin.y + baseHeight
    override def minZ: Double = origin.z
    override def maxZ: Double = origin.z + height
    override def move(dx: Double, dy: Double, dz: Double): Triangle3D = Triangle3D(origin.move(dx, dy, dz), base, baseHeight, height)
    override def surfaceArea: Double = ???
    override def volume: Double = (1/3.0) * ((base + baseHeight) / 2) * height
  }

  def describe[A](x: Shape3D[A]): String = x match {
    case Point3D(x, y, z) => s"Point3D(x = $x, y = $y, z = $z)"
    case Sphere(origin, radius) => s"Sphere(Point3D(x = ${origin.x}, y = ${origin.y}, z = ${origin.z}), radius = $radius)"
    case Cuboid(origin, length, width, height) => s"Cuboid(Point3D(x = ${origin.x}, y = ${origin.y}, z = ${origin.z}), length = $length, width = $width, height = $height)"
    case Cube(origin, length) => s"Cube(Point3D(x = ${origin.x}, y = ${origin.y}, z = ${origin.z}), length = $length)"
//    case Triangle3D(origin, base, baseHeight, height) => s"Triangle3D($origin, base = $base, baseHeight = $baseHeight, height = $height)"
    case Triangle3D(origin, base, baseHeight, height) => s"Triangle3D(Point3D(x = ${origin.x}, y = ${origin.y}, z = ${origin.z}), base = $base, baseHeight = $baseHeight, height = $height)"
  }

}
