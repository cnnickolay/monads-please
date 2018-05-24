package org.nikosoft

import iotaz.TListK.:::
import iotaz.{CopK, TNilK}
import scalaz._

object FreeMonadWithOMGSharedState extends App {

  trait Calculator[A]
  case class Summ(i: Int) extends Calculator[Unit]
  case class Total() extends Calculator[Int]

  trait Logger[A]
  case class Debug() extends Logger[Unit]

  type MyState[A] = State[Int, A]

  implicit val CalculatorInterpreter: Calculator ~> MyState = new (Calculator ~> MyState) {
    override def apply[A](fa: Calculator[A]): MyState[A] = fa match {
      case Summ(i) => State[Int, Unit](total => (total + i, ()))
      case Total() => State[Int, Int](total => (total, total))
    }
  }

  implicit val LoggerInterpreter: Logger ~> MyState = new (Logger ~> MyState) {
    override def apply[A](fa: Logger[A]): MyState[A] = fa match {
      case Debug() => State[Int, Unit](total => {println(s"Total is $total"); (total, Unit)})
    }
  }

  type Algebra[A] = CopK[Calculator ::: Logger ::: TNilK, A]

  implicit class AlgebraSyntax[F[_], A](fa: F[A])(implicit ev: CopK.Inject[F, Algebra]) {
    def liftFree: Free[Algebra, A] = Free.liftF(fa).mapSuspension(ev.inj)
  }

  val interpreter: scalaz.NaturalTransformation[Algebra, MyState] = CopK.NaturalTransformation.of[Algebra, MyState](CalculatorInterpreter, LoggerInterpreter)

  val f = for {
    _       <- Summ(10).liftFree
    _       <- Summ(10).liftFree
    _       <- Summ(1).liftFree
    _       <- Debug().liftFree
    result  <- Total().liftFree
  } yield result

  val result: State[Int, Int] = f.foldMap(interpreter)

  println(result.eval(0))

}
