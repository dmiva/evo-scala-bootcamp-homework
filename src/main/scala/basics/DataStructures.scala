package basics

object DataStructures {

  // Homework
  //
  // Implement a special sort which sorts the keys of a map (K) according to their associated
  // values (V).
  //
  // In case of "ties" (equal values) it should group these keys K into Set-s in the results.
  //
  // The input is a map where keys (K) are the values to be sorted and values are their associated numeric
  // values.
  //
  // The output is a list (in ascending order according to the associated `Int` values) of tuples of `Set`-s
  // with values from K, and the associated value V for these values in the `Set`.
  //
  // For example:
  //
  // Input `Map("a" -> 1, "b" -> 2, "c" -> 4, "d" -> 1, "e" -> 0, "f" -> 2, "g" -> 2)` should result in
  // output `List(Set("e") -> 0, Set("a", "d") -> 1, Set("b", "f", "g") -> 2, Set("c") -> 4)`.
  def sortConsideringEqualValues[T](map: Map[T, Int]): List[(Set[T], Int)] = ???

  def main(args: Array[String]): Unit = {

    val myMap = Map("a" -> 1, "b" -> 2, "c" -> 4, "d" -> 1, "e" -> 0, "f" -> 2, "g" -> 2)

    val myList = List(Set("e") -> 0, Set("a", "d") -> 1, Set("b", "f", "g") -> 2, Set("c") -> 4)

    println("Map: " + myMap)
    println("Expected: " + myList)


//    val mapCollect = myMap.toList.collect({
//      case (key, value) => (value, key)
//    })



//    println("Collected: " + mapCollect)
//    println("Sorted : " + mapCollect.sorted)


//    val groupedList = mapCollect.sorted.groupBy(_._1)
//    println("Grouped : " + groupedList)

    val mapSwapped = myMap.groupBy(_._2).map {
      case (k, v) => k -> v.keys.toList.sorted.toSet
    }.collect({
      case (key, value) => (value, key)
    }).toList.sortBy(_._2)

    println("Swapped: " + mapSwapped)

//    val myPartFunc: PartialFunction[(String,Int),(Int,String)] = new PartialFunction[(String,Int),(Int,String)] {
//      override def isDefinedAt(x: (String, Int)): Boolean = true
//
//      override def apply(v1: (String, Int)): (Int, String) = v1 match {
//        case (str, i) => (i, str)
//      }
//    }

    //    println(mapCollect.sorted.groupBy(_._1))

//
//    })
//    val sortedList = myList.sortBy(_._2)
//

//
//    val reverseMap: PartialFunction[(String, Int), (Int, String)] = {
//      case (str, i) => (i, str)
//    }
//
//    println(sortedList)
  }

}
