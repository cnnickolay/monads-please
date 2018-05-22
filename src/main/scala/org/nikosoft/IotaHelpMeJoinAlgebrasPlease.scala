package org.nikosoft

import iotaz.TListK.:::
import iotaz._
import scalaz._
import Scalaz._

import scala.collection.parallel.Task
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
      case PrintToConsole(msg) => println("test");println(msg); "done".some.asInstanceOf[Option[A]]
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

  implicit class AlgebraSyntax[F[_], A](fa: F[A])(
    implicit ev: CopK.Inject[F, Algebra]
  ) {
    def liftFree: Free[Algebra, A] = Free.liftF(fa).mapSuspension(ev.inj)
  }

  object Algebra {
    def startTransaction() = StartTransaction().liftFree
    def printToConsole(message: String) = PrintToConsole(message).liftFree
  }

  import Algebra._

  val func = for {
    _ <- startTransaction()
    msg <- printToConsole("Liiiiiveeeeeee")
    _ <- LogDebug(s"It's aliiiivvvveeeeee $msg").liftFree
    _ <- true.option(CommitTransaction().liftFree).sequenceU
    res <- true.option(printToConsole("Liiiiiveeeeeee")).sequenceU
    msg2 <- List(printToConsole("Liiiiiveeeeeee")).sequenceU
  } yield msg

  val func2 = for {
    _ <- func
    msg <- printToConsole("Liiiiiveeeeeee")
  } yield msg

  val interpreter: scalaz.NaturalTransformation[Algebra, Option] = CopK.NaturalTransformation.summon[Algebra, Option]

  val str = func2.foldMap(interpreter)
  println(str)

}
