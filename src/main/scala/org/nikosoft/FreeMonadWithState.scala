package org.nikosoft

import iotaz.TListK.:::
import iotaz.{CopK, TNilK}
import scalaz._

object FreeMonadWithState extends App {

  trait Calculator[A]
  case class Summ(i: Int) extends Calculator[Unit]
  case class Total() extends Calculator[Int]

  type CalculatorState[A] = State[Int, A]
  implicit val CalculatorInterpreter: Calculator ~> CalculatorState = new (Calculator ~> CalculatorState) {
    override def apply[A](fa: Calculator[A]): CalculatorState[A] = fa match {
      case Summ(i) => State[Int, Unit](total => (total + i, ()))
      case Total() => State[Int, Int](total => (total, total))
    }
  }

  type Algebra[A] = CopK[Calculator ::: TNilK, A]

  implicit class AlgebraSyntax[F[_], A](fa: F[A])(
    implicit ev: CopK.Inject[F, Algebra]
  ) {
    def liftFree: Free[Algebra, A] = Free.liftF(fa).mapSuspension(ev.inj)
  }

  val interpreter: scalaz.NaturalTransformation[Algebra, CalculatorState] = CopK.NaturalTransformation.summon[Algebra, CalculatorState]

  val f = for {
    _       <- Summ(10).liftFree
    _       <- Summ(10).liftFree
    _       <- Summ(1).liftFree
    result  <- Total().liftFree
  } yield result

  val result: State[Int, Int] = f.foldMap(interpreter)

  println(result.eval(0))

}
