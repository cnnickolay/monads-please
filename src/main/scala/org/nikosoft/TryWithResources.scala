package org.nikosoft

import scalaz.{StateT, _}
import Scalaz._

import scala.util
import scala.util.Try

object TryWithResources extends App {

  type TryWithResource[T] = StateT[({type λ[A] = Either[Throwable, A]})#λ, List[Resource[_]], T]

  object TryWithResource {
    def apply[T](func: => T)(closeResource: T => Unit = (_: T) => ()): TryWithResource[T] = StateT[({type λ[A] = Either[Throwable, A]})#λ, List[Resource[_]], T] { state =>
      Try(func) match {
        case util.Success(value) =>
          val newResource = new Resource[T](value, closeResource)
          Right((newResource +: state, value))
        case util.Failure(t) =>
          state.foreach(_.close())
          Left(t)
      }
    }
  }

  class Resource[A](val value: A, val closeFunc: A => Unit) {
    def close() = closeFunc(value)
  }

  implicit class WrapTry[T](tryWithResource: Try[T]) {
    def res(closeResource: T => Unit = (_: T) => ()) = StateT[({type λ[A] = Either[Throwable, A]})#λ, List[Resource[_]], T] { state =>
      tryWithResource match {
        case util.Success(value) =>
          val newResource = new Resource[T](value, closeResource)
          Right((newResource +: state, value))
        case util.Failure(t) =>
          state.foreach(_.close())
          Left(t)
      }
    }

    def noRes = res(_ => ())
  }

  implicit class Evaluate[T](tryWithResource: TryWithResource[T]) {
    def safeEval: Throwable \/ T = {
      val result = tryWithResource.run(Nil).map { case (state, value) =>
        state.foreach(_.close())
        value
      }
      Disjunction.fromEither(result)
    }
  }

  val result = for {
    a <- Try(5).noRes
    b <- Try(19).recover { case t => 10 }.noRes
    c <- Try(20).res { x => println(x - 10)}
  } yield a + b + c

  result.safeEval
}
