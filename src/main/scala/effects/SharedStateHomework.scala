package effects

import cats.Monad
import cats.effect.concurrent.Ref
import cats.effect.{Clock, Concurrent, ExitCode, IO, IOApp, Timer}

import cats.implicits._
import scala.concurrent.duration._

/*
 * Please implement a Cache which allows concurrent access.
 *
 * Tip: checking expiration could be represented as some infinite process somewhere in background
 *
 * Tip: you can use following structure to get current time suspended in effect : Clock[F].realTime(MILLISECONDS).flatMap(...)
 *
 * Cached items should have an expiration timestamp after which they are evicted.
 *
 * If we will put a value with the same key then it should renew expiration
 */
object SharedStateHomework extends IOApp {
  trait Cache[F[_], K, V] {
    def get(key: K): F[Option[V]]

    def put(key: K, value: V): F[Unit]
  }

  class RefCache[F[_] : Clock : Monad, K, V](
                                              state: Ref[F, Map[K, (Long, V)]],
                                              expiresIn: FiniteDuration
                                            ) extends Cache[F, K, V] {

    def get(key: K): F[Option[V]] = state.get.map(_.get(key).map { case (long, v) => v })

    //    def put(key: K, value: V): F[Unit] = state.update { map =>
    //      Clock[F].realTime(MILLISECONDS).flatMap { time =>
    //
    //      }
    //      map + (key -> (time, value))
    //
    //    }

    //    def put(key: K, value: V): F[Unit] = {
    //      for {
    //        time <- Clock[F].realTime(MILLISECONDS)
    //      } yield state.update { map => map + (key -> (time, value)) }
    //    }
    //  }

    def put(key: K, value: V): F[Unit] = {
      Clock[F].realTime(MILLISECONDS).flatMap { time =>
//        println(s"Put Key: $key, ExpTime: ${time+expiresIn.toMillis}, CurrTime: $time")
        state.update(map => map + (key -> (time + expiresIn.toMillis, value)))
      }
    }

  }


  object Cache {
    def of[F[_] : Clock, K, V](
                                expiresIn: FiniteDuration,
                                checkOnExpirationsEvery: FiniteDuration
                              )(implicit T: Timer[F], C: Concurrent[F]): F[Cache[F, K, V]] = {
      //      Ref.of[F, Map[K, (Long, V)]](): F[Cache[F, K, V]]

      def cleanUp(state: Ref[F, Map[K, (Long, V)]]): F[Unit] = {
        //
        //        val currentTime = Clock[F].realTime(MILLISECONDS)
        //         val notExpiredElems = state.get.map(_.filter(_._2._1 > currentTime))

        val cleanCache = Clock[F].realTime(MILLISECONDS).flatMap { currentTime =>
//          println(s"Time: $currentTime")
          state.update(map => map.collect {
            case elem @ (key, (expTime, v)) if expTime > currentTime => {
//              println(s"Key: $key, ExpTime: $expTime, CurrTime: $currentTime, Keep?: ${expTime > currentTime}")
              //            case elem @ (key, (expTime, v)) => {
              elem
            }
          })
        }
        T.sleep(checkOnExpirationsEvery) >> cleanCache >> cleanUp(state)
//        T.sleep(checkOnExpirationsEvery) >> cleanUp(state)
      }

      //      val cache = Ref.of[F, Map[K, (Long, V)]](Map.empty)
      Ref.of[F, Map[K, (Long, V)]](Map.empty).flatTap(s => C.start(cleanUp(s)).void).map(ref => new RefCache[F, K, V](ref, expiresIn))
      //      Ref.of[F, Map[K, (Long, V)]](Map.empty).map { ref =>
      //        new RefCache[F, K, V](ref, expiresIn)
      //      }

    }
  }

  override def run(args: List[String]): IO[ExitCode] = {

    for {
      cache <- Cache.of[IO, Int, String](10.seconds, 4.seconds)
      _ <- cache.put(1, "Hello")
      _ <- cache.put(2, "World")
      _ <- cache.get(1).flatMap(s => IO {
        println(s"first key $s")
      })
      _ <- cache.get(2).flatMap(s => IO {
        println(s"second key $s")
      })
      _ <- IO.sleep(12.seconds)
      _ <- cache.get(1).flatMap(s => IO {
        println(s"first key $s")
      })
      _ <- cache.get(2).flatMap(s => IO {
        println(s"second key $s")
      })
      _ <- IO.sleep(12.seconds)
      _ <- cache.get(1).flatMap(s => IO {
        println(s"first key $s")
      })
      _ <- cache.get(2).flatMap(s => IO {
        println(s"second key $s")
      })
    } yield ExitCode.Success


//    for {
//      cache <- Cache.of[IO, Int, String](10.seconds, 4.seconds)
//      _ <- cache.put(1, "value1")
//      _ <- cache.put(2, "value2")
//      _ <- cache.get(1).flatMap(s => IO {
//        println(s"first key $s")
//      })
//      _ <- cache.get(2).flatMap(s => IO {
//        println(s"second key $s")
//      })
//      _ <- IO.sleep(8.seconds)
//      _ <- cache.get(1).flatMap(s => IO {
//        println(s"first key $s")
//      })
//      _ <- cache.get(2).flatMap(s => IO {
//        println(s"second key $s")
//      })
//      _ <- IO.sleep(3.seconds)
//      _ <- cache.get(1).flatMap(s => IO {
//        println(s"first key $s")
//      })
//      _ <- cache.get(2).flatMap(s => IO {
//        println(s"second key $s")
//      })
//      _ <- IO.sleep(2.seconds) // Should evict during this sleep
//      _ <- cache.get(1).flatMap(s => IO {
//        println(s"first key $s")
//      })
//      _ <- cache.get(2).flatMap(s => IO {
//        println(s"second key $s")
//      })
//    } yield ExitCode.Success
  }
}

