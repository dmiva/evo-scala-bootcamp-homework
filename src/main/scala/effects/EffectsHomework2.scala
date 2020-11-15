package effects

import cats.effect.IO
import effects.EffectsHomework1.IO

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}
//import effects.EffectsHomework1.IO

  /*
 * Homework 2. Same as homework 1 but in declarative encoding.
 */
  object EffectsHomework2 {

    sealed trait IO[A] {
//      def map[B](f: A => B): IO[B] = IO(f(run()))
//      def map[B](f: A => B): IO[B] = Map(this, f)
//      def flatMap[B](f: A => IO[B]): IO[B] = IO(f(run()).run())
      def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)

//      def *>[B](another: IO[B]): IO[B] = flatMap(_ => another)
//      def as[B](newValue: => B): IO[B] = map(_ => newValue)
//      def void: IO[Unit] = IO(())

//      def attempt: IO[Either[Throwable, A]] = IO(Try(run()) match {
//        case Failure(exception) => Left(exception)
//        case Success(value) => Right(value)
//      })
//
//      def option: IO[Option[A]] = IO(Try(run()) match {
//        case Success(value) => Some(value)
//        case Failure(_) => None
//      })
//
//      def handleErrorWith[AA >: A](f: Throwable => IO[AA]): IO[AA] = IO(Try(run()) match {
//        case Failure(exception) => f(exception).run()
//        case Success(value) => IO[AA](value).run()
//      })
//
//      def redeem[B](recover: Throwable => B, map: A => B): IO[B] = IO(Try(run()) match {
//        case Failure(exception) => recover(exception)
//        case Success(value) => map(value)
//      })
//
//      def redeemWith[B](recover: Throwable => IO[B], bind: A => IO[B]): IO[B] = IO(Try(run()) match {
//        case Failure(exception) => recover(exception).run()
//        case Success(value) => bind(value).run()
//      })

      def unsafeRunSync(): A = run(this)
//      def unsafeToFuture()(implicit ec: ExecutionContext): Future[A] = Future(run())

      @tailrec
      def run[A](io: IO[A]): A = io match {
        case Pure(a) => a
        case Delay(thunk) => thunk()
        case Suspend(thunk) => run(thunk())

        case FlatMap(source, f) => source match {
          case Pure(a) => run(f(a))
          case Delay(thunk) => run(f(thunk()))
//          case Suspend(thunk) => run(FlatMap(thunk(),f))
          case Suspend(thunk) => run(f(thunk()  ))
          case FlatMap(source, g) => run(source.flatMap(g(_) flatMap f))
        }
//        case RaiseError(e) =>
//        case Suspend(thunk) =>

//        case Map(source, f) => run(IO(f(source).asInstanceOf[A]))
//        case FlatMap(source, f) => run(source map f)

//        }
//        case Map(source, f) =>
      }
    }
    //  case FlatMap(x, f) => x match {
    //    case Return(a) => run(f(a))
    //    case Suspend(r) => run(f(r()))
    //    case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
    //  }

    object IO {
      def apply[A](body: => A): IO[A] = delay(body)
//      def suspend[A](thunk: => IO[A]): IO[A] = Suspend(() => thunk)
      def delay[A](body: => A): IO[A] = Delay(() => body)
//      def pure[A](a: A): IO[A] = Pure(a)

//      def fromEither[A](e: Either[Throwable, A]): IO[A] = e match {
//        case Left(exception) => raiseError(exception)
//        case Right(value) => pure(value)
//      }
//
//      def fromOption[A](option: Option[A])(orElse: => Throwable): IO[A] = option match {
//        case Some(value) => pure(value)
//        case None => raiseError(orElse)
//      }
//
//      def fromTry[A](t: Try[A]): IO[A] = t match {
//        case Failure(exception) => raiseError(exception)
//        case Success(value) =>pure(value)
//      }

//      def none[A]: IO[Option[A]] = pure(None)
//      def raiseError[A](e: Throwable): IO[A] = delay(throw e)
//      def raiseUnless(cond: Boolean)(e: => Throwable): IO[Unit] = if (cond) unit else raiseError(e)
//      def raiseWhen(cond: Boolean)(e: => Throwable): IO[Unit] = if (cond) raiseError(e) else unit
//      def unlessA(cond: Boolean)(action: => IO[Unit]): IO[Unit] = if (cond) unit else action
//      def whenA(cond: Boolean)(action: => IO[Unit]): IO[Unit] = if (cond) action else unit
//      val unit: IO[Unit] = pure(())

    }

    final case class Pure[A](a: A) extends IO[A]
    final case class Delay[A](thunk: () => A) extends IO[A]
//    final private case class RaiseError(e: Throwable) extends IO[Nothing]
    final case class Suspend[A](thunk: () => IO[A]) extends IO[A]
    final case class FlatMap[A, B](source: IO[A], f: A => IO[B]) extends IO[B]
//    final private case class Map[A, +B](source: IO[A], f: A => B) extends IO[A]


//sealed trait IO2[A] {
//  def flatMap[B](f: A => IO2[B]): IO2[B] =
//    FlatMap(this, f)
//  def map[B](f: A => B): IO2[B] =
//    flatMap(f andThen (Return(_)))
//}
//  case class Return[A](a: A) extends IO2[A]
//  case class Suspend[A](resume: () => A) extends IO2[A]
//  case class FlatMap[A,B](sub: IO2[A], k: A => IO2[B]) extends IO2[B]
//
//
//@annotation.tailrec def run[A](io: IO2[A]): A = io match {
//  case Return(a) => a
//  case Suspend(r) => r()
//  case FlatMap(x, f) => x match {
//    case Return(a) => run(f(a))
//    case Suspend(r) => run(f(r()))
//    case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
//  }
//}



  def main(args: Array[String]): Unit = {

//    val a = IO("aaa")
//    val c = a.flatMap(_ => "ccc")
//    c.unsafeRunSync()

    def hello(text: IO[Unit], n: Int): IO[Unit] = {
      if (n > 0) hello(text.flatMap(_ => text), n-1)
      else text
    }

    val tt = hello(IO(println("Survived!")), 10)
    tt.unsafeRunSync()
  }
}
