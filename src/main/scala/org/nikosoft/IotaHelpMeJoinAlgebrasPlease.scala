package org.nikosoft

import java.util.concurrent.{ExecutorService, Executors}

import iotaz.TListK.:::
import iotaz._
import scalaz._
import Scalaz._
import scalaz.concurrent.Task

import scala.language.higherKinds


/**
  * Created by Nikolay Cherkezishvili on 10/05/2018
  */
object IotaHelpMeJoinAlgebrasPlease extends App {
  private val service: ExecutorService = Executors.newFixedThreadPool(2)

  trait Printer[A]
  case class PrintToConsole(message: String) extends Printer[String]

  trait Logger[A]
  case class LogDebug(message: String) extends Logger[Unit]

  trait Transaction[A]
  case class StartTransaction() extends Transaction[Unit]
  case class CommitTransaction() extends Transaction[Unit]

  type Algebra[A] = CopK[Printer ::: Logger ::: Transaction ::: TNilK, A]


  implicit val PrinterInterpreter: Printer ~> Task = new (Printer ~> Task) {
    override def apply[A](fa: Printer[A]): Task[A] = fa match {
      case PrintToConsole(msg) => Task {
        println(s">>> ${Thread.currentThread.getName}")
        Thread.sleep(1000)
        println("<<<")
        msg
      }.asInstanceOf[Task[A]]
    }
  }

  implicit val LoggerInterpreter: Logger ~> Task = new (Logger ~> Task) {
    override def apply[A](fa: Logger[A]): Task[A] = fa match {
      case LogDebug(msg) => println(s"Logging $msg"); Task(()).asInstanceOf[Task[A]]
    }
}

  implicit val TransactionInterpreter: Transaction ~> Task = new (Transaction ~> Task) {
    override def apply[A](fa: Transaction[A]): Task[A] = fa match {
      case StartTransaction() => println("transaction started"); Task(()).asInstanceOf[Task[A]]
      case CommitTransaction() => println("transaction committed"); Task(()).asInstanceOf[Task[A]]
    }
  }

  val interpreter: scalaz.NaturalTransformation[Algebra, Task] = CopK.NaturalTransformation.summon[Algebra, Task]

  implicit val applicative = new Applicative[({type T[A] = Free[Algebra, A]})#T] {
    override def point[A](a: => A): Free[Algebra, A] = Free.pure[Algebra, A](a)

    def ap[A,B](a: => Free[Algebra, A])(f: => Free[Algebra, A => B]): Free[Algebra, B] = apply2(f,a)(_(_))

    override def apply2[A, B, C](a: => Free[Algebra, A], b: => Free[Algebra, B])(f: (A, B) => C): Free[Algebra, C] = {
      val res = Nondeterminism[Task].mapBoth(Task.fork(a.foldMap(interpreter))(service), Task.fork(b.foldMap(interpreter))(service))(f)
      Free.pure(res.unsafePerformSync)
    }
  }
  /*
    implicit val applicative = new Apply[({type T[A] = Free[Algebra, A]})#T] {
      override def ap[A, B](fa: => Free[Algebra, A])(f: => Free[Algebra, A => B]): Free[Algebra, B] = {
        println("1")

        for {
          _fa <- fa
          _ = println("2 " + _fa)
          _f <- f
        } yield _f(_fa)
      }

      override def map[A, B](fa: Free[Algebra, A])(f: A => B): Free[Algebra, B] = fa map f
    }
  */

  class Printers[T[A] <: CopK[_, A]](implicit ev: CopK.Inject[Printer, T]) {
    def printToConsole(message: String) = Free.liftF(PrintToConsole(message)).mapSuspension(ev.inj)
  }
  object Printers {
    implicit def instantiate[T[A] <: CopK[_, A]](implicit I: CopK.Inject[Printer, T]) = new Printers[T]
  }

  class Loggers[T[A] <: CopK[_, A]](implicit ev: CopK.Inject[Logger, T]) {
    def logDebug(message: String) = Free.liftF(LogDebug(message)).mapSuspension(ev.inj)
  }
  object Loggers {
    implicit def instantiate[T[A] <: CopK[_, A]](implicit I: CopK.Inject[Logger, T]) = new Loggers[T]
  }

  def p(implicit P: Printers[Algebra], L: Loggers[Algebra]) = {
    import P._, L._

    for {
//      s <- printToConsole("hello")
//      s2 <- printToConsole(s + "_there")
//      _ <- logDebug(s2)
      ressss <- (printToConsole("1") |@| printToConsole(" 2")) (_ + _)
      resss <- (printToConsole(ressss) |@| printToConsole(" 3")) (_ + _)
    } yield resss
  }

  println(Task.fork(p.foldMap(interpreter))(service).unsafePerformSync)
//  println(p.foldMap(interpreter).unsafePerformSync)

}
