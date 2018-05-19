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
      case PrintToConsole(msg) => println(msg); "done".some.asInstanceOf[Option[A]]
    }
  }

  implicit val LoggerInterpreter: Logger ~> Option = new (Logger ~> Option) {
    override def apply[A](fa: Logger[A]): Option[A] = fa match {
      case LogDebug(msg) => println(s"Logging $msg"); None
    }
  }

  implicit val TransactionInterpreter: Transaction ~> Option = new (Transaction ~> Option) {
    override def apply[A](fa: Transaction[A]): Option[A] = fa match {
      case StartTransaction() => println("transaction started"); None
      case CommitTransaction() => println("transaction committed"); None
    }
  }

  implicit class AlgebraSyntax[F[_], A](fa: F[A])(
    implicit ev: CopK.Inject[F, Algebra]
  ) {
    def liftFree: Free[Algebra, A] = Free.liftF(fa).mapSuspension(ev.inj)
  }

  val func: Free[Algebra, Unit] = for {
    _ <- StartTransaction().liftFree
    msg <- PrintToConsole("Liiiiiveeeeeee").liftFree
    _ <- LogDebug(s"It's aliiiivvvveeeeee $msg").liftFree
    _ <- CommitTransaction().liftFree
  } yield ()

  val interpreter: scalaz.NaturalTransformation[Algebra, Option] = CopK.NaturalTransformation.summon[Algebra, Option]

  func.foldMap(interpreter)

}
