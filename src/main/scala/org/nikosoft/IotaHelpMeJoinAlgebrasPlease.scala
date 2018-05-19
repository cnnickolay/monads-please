package org.nikosoft

import iotaz.TListK.:::
import iotaz._

import scalaz._


/**
  * Created by Nikolay Cherkezishvili on 10/05/2018
  */
object IotaHelpMeJoinAlgebrasPlease extends App {

  trait Printer[A]
  case class PrintToConsole(message: String) extends Printer[String]

  trait Logger[A]
  case class LogDebug(message: String) extends Logger[Unit]

  type Algebra[A] = CopK[Printer ::: Logger ::: TNilK, A]

  implicit val PrinterInterpreter: (Printer ~> Id.Id) = new (Printer ~> Id.Id) {
    override def apply[A](fa: Printer[A]): Id.Id[A] = fa match {
      case PrintToConsole(msg) => println(msg); "done"
    }
  }

  implicit val LoggerInterpreter: (Logger ~> Id.Id) = new (Logger ~> Id.Id) {
    override def apply[A](fa: Logger[A]): Id.Id[A] = fa match {
      case LogDebug(msg) => println(s"Logging $msg"); ()
    }
  }

  implicit class AlgebraSyntax[F[_], A](fa: F[A])(
    implicit ev: CopK.Inject[F, Algebra]
  ) {
    def liftFree: Free[Algebra, A] = Free.liftF(fa).mapSuspension(ev.inj)
  }

  val func: Free[Algebra, Unit] = for {
    msg <- PrintToConsole("Liiiiiveeeeeee").liftFree
    _ <- LogDebug(s"It's aliiiivvvveeeeee $msg").liftFree
  } yield ()

  val interpreter: scalaz.NaturalTransformation[Algebra, Id.Id] = CopK.NaturalTransformation.summon[Algebra, Id.Id]

  func.foldMap(interpreter)

}
