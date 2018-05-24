package org.nikosoft

import iotaz.TListK.:::
import iotaz._
import scalaz._
import Scalaz._

import scala.language.higherKinds


/**
  * Created by Nikolay Cherkezishvili on 10/05/2018
  */
object IotaHelpMeJoinAlgebrasPlease extends App {

  trait Printer[A]
  case class PrintToConsole(message: String) extends Printer[String]

  trait Logger[A]
  case class LogDebug(message: String) extends Logger[Unit]

  trait Transaction[A]
  case class StartTransaction() extends Transaction[Unit]
  case class CommitTransaction() extends Transaction[Unit]

  type Algebra[A] = CopK[Printer ::: Logger ::: Transaction ::: TNilK, A]

  implicit val PrinterInterpreter: Printer ~> Option = new (Printer ~> Option) {
    override def apply[A](fa: Printer[A]): Option[A] = fa match {
      case PrintToConsole(msg) => msg.some.asInstanceOf[Option[A]]
    }
  }

  implicit val LoggerInterpreter: Logger ~> Option = new (Logger ~> Option) {
    override def apply[A](fa: Logger[A]): Option[A] = fa match {
      case LogDebug(msg) => println(s"Logging $msg"); Option(()).asInstanceOf[Option[A]]
    }
}

  implicit val TransactionInterpreter: Transaction ~> Option = new (Transaction ~> Option) {
    override def apply[A](fa: Transaction[A]): Option[A] = fa match {
      case StartTransaction() => println("transaction started"); Option(()).asInstanceOf[Option[A]]
      case CommitTransaction() => println("transaction committed"); Option(()).asInstanceOf[Option[A]]
    }
  }

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

  val interpreter: scalaz.NaturalTransformation[Algebra, Option] = CopK.NaturalTransformation.summon[Algebra, Option]

  def p(implicit P: Printers[Algebra], L: Loggers[Algebra]) = {
    import P._, L._

    for {
      s <- printToConsole("hello")
      s2 <- printToConsole(s + "_there")
      _ <- logDebug(s2)
    } yield s2
  }

  println(p.foldMap(interpreter))

}
