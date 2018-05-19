package org.nikosoft

import iotaz.TListK.:::
import iotaz._
import scalaz.{Id, _}

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

  implicit val PrinterInterpreter: Printer ~> Id.Id = new (Printer ~> Id.Id) {
    override def apply[A](fa: Printer[A]): Id.Id[A] = fa match {
      case PrintToConsole(msg) => println(msg); "done"
    }
  }

  implicit val LoggerInterpreter: Logger ~> Id.Id = new (Logger ~> Id.Id) {
    override def apply[A](fa: Logger[A]): Id.Id[A] = fa match {
      case LogDebug(msg) => println(s"Logging $msg"); ()
    }
  }

  implicit val TransactionInterpreter: Transaction ~> Id.Id = new (Transaction ~> Id.Id) {
    override def apply[A](fa: Transaction[A]): Id.Id[A] = fa match {
      case StartTransaction() => println("transaction started")
      case CommitTransaction() => println("transaction committed")
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

  val interpreter: scalaz.NaturalTransformation[Algebra, Id.Id] = CopK.NaturalTransformation.summon[Algebra, Id.Id]

  func.foldMap(interpreter)

}
