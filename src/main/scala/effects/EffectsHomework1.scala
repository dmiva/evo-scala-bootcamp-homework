package effects

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/*
 * Homework 1. Provide your own implementation of a subset of `IO` functionality.
 *
 * Provide also tests for this functionality in EffectsHomework1Spec (which you should create).
 *
 * Refer to:
 *  - https://typelevel.org/cats-effect/datatypes/io.html
 *  - https://typelevel.org/cats-effect/api/cats/effect/IO$.html
 *  - https://typelevel.org/cats-effect/api/cats/effect/IO.html
 * about the meaning of each method as needed.
 *
 * There are two main ways how to implement IO:
 * - Executable encoding  - express every constructor and operator for our model in terms of its execution
 * - Declarative encoding - express every constructor and operator for our model as pure data in a recursive
 *                          tree structure
 *
 * While the real Cats Effect IO implementation uses declarative encoding, it will be easier to solve this
 * task using executable encoding, that is:
 *  - Add a `private val run: () => A` parameter to the class `IO` private constructor
 *  - Have most of the methods return a `new IO(...)`
 *
 * Ask questions in the bootcamp chat if stuck on this task.
 */
object EffectsHomework1 {
  final class IO[A] private (val run: () => A) {

    def map[B](f: A => B): IO[B] = IO(f(run()))
    def flatMap[B](f: A => IO[B]): IO[B] = IO(f(run()).run())
    def *>[B](another: IO[B]): IO[B] = flatMap(_ => another)
    def as[B](newValue: => B): IO[B] = map(_ => newValue)
    def void: IO[Unit] = IO(())

    def attempt: IO[Either[Throwable, A]] = IO(Try(run()) match {
      case Failure(exception) => Left(exception)
      case Success(value) => Right(value)
    })

    def option: IO[Option[A]] = IO(Try(run()) match {
      case Success(value) => Some(value)
      case Failure(_) => None
    })

    def handleErrorWith[AA >: A](f: Throwable => IO[AA]): IO[AA] = IO(Try(run()) match {
      case Failure(exception) => f(exception).run()
      case Success(value) => IO[AA](value).run()
    })

    def redeem[B](recover: Throwable => B, map: A => B): IO[B] = IO(Try(run()) match {
      case Failure(exception) => recover(exception)
      case Success(value) => map(value)
    })

    def redeemWith[B](recover: Throwable => IO[B], bind: A => IO[B]): IO[B] = IO(Try(run()) match {
      case Failure(exception) => recover(exception).run()
      case Success(value) => bind(value).run()
    })

    def unsafeRunSync(): A = run()
    def unsafeToFuture()(implicit ec: ExecutionContext): Future[A] = Future(run())
  }

  object IO {
    def apply[A](body: => A): IO[A] = delay(body)
    def suspend[A](thunk: => IO[A]): IO[A] = thunk
    def delay[A](body: => A): IO[A] = new IO(() => body)
    def pure[A](a: A): IO[A] = IO(a)

    def fromEither[A](e: Either[Throwable, A]): IO[A] = e match {
      case Left(exception) => raiseError(exception)
      case Right(value) => pure(value)
    }

    def fromOption[A](option: Option[A])(orElse: => Throwable): IO[A] = option match {
      case Some(value) => pure(value)
      case None => raiseError(orElse)
    }

    def fromTry[A](t: Try[A]): IO[A] = t match {
      case Failure(exception) => raiseError(exception)
      case Success(value) =>pure(value)
    }

    def none[A]: IO[Option[A]] = pure(None)
    def raiseError[A](e: Throwable): IO[A] = delay(throw e)
    def raiseUnless(cond: Boolean)(e: => Throwable): IO[Unit] = if (cond) unit else raiseError(e)
    def raiseWhen(cond: Boolean)(e: => Throwable): IO[Unit] = if (cond) raiseError(e) else unit
    def unlessA(cond: Boolean)(action: => IO[Unit]): IO[Unit] = if (cond) unit else action
    def whenA(cond: Boolean)(action: => IO[Unit]): IO[Unit] = if (cond) action else unit
    val unit: IO[Unit] = pure(())
  }

}