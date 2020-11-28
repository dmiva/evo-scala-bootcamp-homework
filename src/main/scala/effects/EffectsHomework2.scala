package effects

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Left, Right, Success, Try}

  /*
 * Homework 2. Same as homework 1 but in declarative encoding.
 * Map, flatMap, *>, as - are stack safe
 */
object EffectsHomework2 {

  sealed trait IO[A] {
    def map[B](f: A => B): IO[B] = flatMap(a => Pure(f(a)))
    def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)
    def *>[B](another: IO[B]): IO[B] = flatMap(_ => another)
    def as[B](newValue: => B): IO[B] = map(_ => newValue)
    def void: IO[Unit] = IO(())

    def attempt: IO[Either[Throwable, A]] = IO(Try(run(this)) match {
      case Failure(exception) => Left(exception)
      case Success(value) => Right(value)
    })

//    def attempt: IO[Either[Throwable, A]] = FlatMap(this, AttemptIO.asInstanceOf[A => IO[Either[Throwable, A]]])
//    def attempt: IO[Either[Throwable, A]] = Attempt(this)

    def option: IO[Option[A]] = IO(Try(run(this)) match {
      case Success(value) => Some(value)
      case Failure(_) => None
    })

    def handleErrorWith[AA >: A](f: Throwable => IO[AA]): IO[AA] = IO(Try(run(this)) match {
      case Failure(exception) => run(f(exception))
      case Success(value) => run(IO[AA](value))
    })

    def redeem[B](recover: Throwable => B, map: A => B): IO[B] = IO(Try(run(this)) match {
      case Failure(exception) => recover(exception)
      case Success(value) => map(value)
    })

    def redeemWith[B](recover: Throwable => IO[B], bind: A => IO[B]): IO[B] = IO(Try(run(this)) match {
      case Failure(exception) => run(recover(exception))
      case Success(value) => run(bind(value))
    })

    def unsafeRunSync(): A = run(this)
    def unsafeToFuture()(implicit ec: ExecutionContext): Future[A] = Future(run(this))

    @tailrec
    def run[A](io: IO[A]): A = io match {
      case Pure(a) => a
      case Delay(thunk) => thunk()
      case Suspend(thunk) => run(thunk())
//      case Attempt(body) => Try(run(body)) match {
//        case Failure(exception) => run(Pure(Left(exception)))
//        case Success(value) => run(Pure(Right(value)))
//      }
      case FlatMap(source, f) => source match {
        case Pure(a) => run(f(a))
        case Delay(thunk) => run(f(thunk()))
//        case Attempt(body) => run(source.flatMap(body) flatMap f)
        case Suspend(thunk) => run(f(thunk()  ))
        case FlatMap(source, g) => run(source.flatMap(g(_) flatMap f))
      }
    }
  }

  final case class Pure[A](a: A) extends IO[A]
  final case class Delay[A](thunk: () => A) extends IO[A]
  final case class Suspend[A](thunk: () => IO[A]) extends IO[A]
  final case class FlatMap[A, B](source: IO[A], f: A => IO[B]) extends IO[B]
//    final case class Attempt[A](body: IO[A]) extends IO[Either[Throwable, A]]
//  final case class Attempt[A](a: Either[Throwable, A]) extends IO[A]
//  }

//    object AttemptIO extends AttemptIOFrame[Any, IO[Either[Throwable, Any]]] {
//      override def apply(a: Any): IO[Either[Throwable, Any]] = Pure(Right(a))
//      override def recover(e: Throwable): IO[Either[Throwable, Any]] = Pure(Left(e))
//    }
//
//    abstract class AttemptIOFrame[-A, +R] extends (A => R) {
//      def apply(a: A): R
//      def recover(e: Throwable): R
//
//      final def fold(value: Either[Throwable, A]): R =
//        value match {
//          case Right(a) => apply(a)
//          case Left(e)  => recover(e)
//        }
//    }

  object IO {
    def apply[A](body: => A): IO[A] = delay(body)
    def suspend[A](thunk: => IO[A]): IO[A] = Suspend(() => thunk)
    def delay[A](body: => A): IO[A] = Delay(() => body)
    def pure[A](a: A): IO[A] = Pure(a)

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
      case Success(value) => pure(value)
    }

    def none[A]: IO[Option[A]] = pure(None)
    def raiseError[A](e: Throwable): IO[A] = delay(throw e)
    def raiseUnless(cond: Boolean)(e: => Throwable): IO[Unit] = if (cond) unit else raiseError(e)
    def raiseWhen(cond: Boolean)(e: => Throwable): IO[Unit] = if (cond) raiseError(e) else unit
    def unlessA(cond: Boolean)(action: => IO[Unit]): IO[Unit] = if (cond) unit else action
    def whenA(cond: Boolean)(action: => IO[Unit]): IO[Unit] = if (cond) action else unit
    val unit: IO[Unit] = pure(())
  }

  def main(args: Array[String]): Unit = {
    val numberOfFunctionCalls = 10000

    def flatMapTesting(text: IO[Unit], n: Int): IO[Unit] = {
      if (n > 0) flatMapTesting(text.flatMap(_ => IO(println(n))), n-1)
      else text
    }

    val input = flatMapTesting(IO(println("Survived!")), numberOfFunctionCalls)
    input.unsafeRunSync()
  }

}
