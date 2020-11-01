package cats

import cats.data.Nested
import cats.implicits._

object NestedExplained extends App {

  // Sometimes you can end up with data inside nested monads, e.g. an integer inside an Either,
  // which in turn is nested inside an Option, or Future, or IO...:
  val optionEither: Option[Either[String, Int]] = Some(Right(123))

  // Or you can have just a list of Options:
  val listOption: List[Option[Int]] = List(Some(1), None, Some(2))

  // Accessing the integer value can be tedious and annoying, because you have to traverse the nested structure,
  // by doing multiple map operations, e.g.:

  // You have to use map operation two times:
  listOption.map(_.map(_ + 1))
  // List(Some(2), None, Some(3))

  // Alternative way of doing same task (call map of Functor's companion object)
  Functor[List].map(listOption)(_.map(_ + 1))

  // Nested can help with this by composing the two map operations into one:
  Nested(listOption).map(_ + 1).value
  // List(Some(2), None, Some(3))

  // You probably are familiar with monad transformers such as OptionT, or EitherT.
  // As example, OptionT has class signature where one out of two type parameters is defined,
  // e.g. OptionT[F[_], A](value: F[Option[A]])

  // Nested is basically the same Monad Transformer, but in a generalized form.
  // It's class signature is:
  // Nested[F[_], G[_], A](value: F[G[A]])

  //  The three type parameters are:
  //  • F[_] is the outer monad in the stack;
  //  • G[_] is the inner monad in the stack;
  //  • A is the result type.

  // Example
  val nested: Nested[List, Option, Int] = Nested(listOption)

  val result: Nested[List, Option, Int] = nested.map(_ + 1)
  // Nested(List(Some(2), None, Some(3)))

  val result2: List[Option[Int]] = nested.map(_ + 1).value
  // List(Some(2), None, Some(3))

  val result3: Nested[List, Option, Int] = Functor[Nested[List, Option, *]].map(nested)(_ + 1)
  // Nested(List(Some(2), None, Some(3)))

  // Based on properties of F[_] and G[_], there are inference rules, which define, what can be produced:
    // If F[_] and G[_] are both Functors, then Nested[F, G, *] is also a Functor (we saw this in action in the example above)
    // Full list of rules is defined in Nested companion object.
    // In order to '*' syntax would work, a compiler plugin Kind Projector must be added to project


  // A more interesting usage of Nested is when you have a function that performs a computation on a value of type A,
  // which appears to be nested inside two monads. For example Either[String, A] which returns a result
  // after synchronous or asynchronous computation. That results in a return type IO[Either[String, A]].
  // For simplicity, we will replace IO with Option, thus output type is Option[Either[String, A]]

  case class UserInfo(name: String, age: Int)
  case class User(id: String, name: String, age: Int)

  val userInfos = List(
    UserInfo("Alice", 22),
    UserInfo("Bob", 30),
    UserInfo("Carol", 44)
  )

  // Imagine we have an API for creating user
  def createUser(userInfo: UserInfo): Option[Either[String, User]] =
    Some(Right(User("id #", userInfo.name, userInfo.age)))

  // Now, we want to create a function that creates multiple users, when a list of userInfos is given.
  // For such task we can use traverse function with Nested.
  def createUsers(userInfos: List[UserInfo]): Option[Either[String, List[User]]] =
    userInfos.traverse(userInfo => Nested(createUser(userInfo))).value
  // That results in output:
  // Some(Right(List(
  //        User(id #,Alice,22),
  //        User(id #,Bob,30),
  //        User(id #,Carol,44)
  //        )))
  // This output looks clean. We get an aggregated result in a single Either.

  // If we used traverse without Nested, output would be different because of traverse return type.
  // For this example, given a List[UserInfo] and a function UserInfo => Option[Either[String, User],
  // traverse returns an Option[List[Either[String, User]]].
  def createUsersNotNested(userInfos: List[UserInfo]): Option[List[Either[String, User]]] =
    userInfos.traverse(createUser)
  // That results in output:
  // Some(List(
  //        Right(User(id #,Alice,22)),
  //        Right(User(id #,Bob,30)),
  //        Right(User(id #,Carol,44))
  //        ))
  // This output is less clean and need more efforts to maintain and work with it.

  // Sources:
  // https://typelevel.org/cats/datatypes/nested.html
  // https://typelevel.org/cats/typeclasses/traverse.html
}
