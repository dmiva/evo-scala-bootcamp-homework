package basics

object Basics {

  // Homework. Implement functions that calculate https://en.wikipedia.org/wiki/Lowest_common_denominator and
  // https://en.wikipedia.org/wiki/Greatest_common_divisor for integers.

  def lcm(a: Int, b: Int): Int = {
    if (a == 0 && b == 0) 0
    else (a * b).abs / gcd(a, b)
  }

  // Euclid's algorithm
  def gcd(a: Int, b: Int): Int = {
    if (b == 0) a
    else gcd(b, a % b)
  }

  def main(args: Array[String]): Unit = {
    println(gcd(48,18) == 6)
    println(gcd(48,-18))
    println(gcd(48,180) == 12)
    println(lcm(48,180) == 720)
    println(lcm(0,0))
  }
}
