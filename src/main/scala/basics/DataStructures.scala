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
  def sortConsideringEqualValues[T](map: Map[T, Int]): List[(Set[T], Int)] = {
    if (map.isEmpty) List()
    else
    map
    // 1. Group keys in collection (Map), whose key-value pairs have identical values
    .groupBy { case (_, value) => value }
    // 2. Change values in key-value pairs from Map of tuples to Set of keys
    .map { case (key, value) => key -> value.keys.toSet }
    // 3. Exchange keys with values
    .map { case (key, value) => (value, key) }
    // 4. Convert 'outer' collection from Map to List
    .toList
    // 5. Sort elements in list by their values
    .sortBy { case (_, value) => value }
  }

  //-1. Map("a" -> 1, "b" -> 2, "c" -> 4, "d" -> 1, "e" -> 0, "f" -> 2, "g" -> 2)
  // 0. HashMap(e -> 0, f -> 2, a -> 1, b -> 2, g -> 2, c -> 4, d -> 1)

  // 1. HashMap(0 -> HashMap(e -> 0), 1 -> HashMap(a -> 1, d -> 1), 2 -> HashMap(f -> 2, b -> 2, g -> 2), 4 -> HashMap(c -> 4))
  // 2. HashMap(0 -> Set(e), 1 -> Set(a, d), 2 -> Set(f, b, g), 4 -> Set(c))
  // 3. HashMap(Set(c) -> 4, Set(e) -> 0, Set(f, b, g) -> 2, Set(a, d) -> 1)
  // 4. List((Set(c),4), (Set(e),0), (Set(f, b, g),2), (Set(a, d),1))
  // 5. List((Set(e),0), (Set(a, d),1), (Set(f, b, g),2), (Set(c),4))

}
