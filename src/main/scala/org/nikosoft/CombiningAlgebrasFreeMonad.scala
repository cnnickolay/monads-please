package org.nikosoft

import scalaz._
import Scalaz._
import scalaz.concurrent.Task

object CombiningAlgebrasFreeMonad extends App {

  trait Logger[T]
  case class Debug(message: String) extends Logger[Unit]

  trait Transaction[T]
  case class Commit() extends Transaction[Unit]

  object Logger {
    class Ops[S[_]](implicit s0: Logger :<: S) {
      def debug(message: String) = Free.liftF(s0.inj(Debug(message)))
    }

    object Ops {
      implicit def apply[S[_]](implicit S: Logger :<: S) = new Ops[S]
    }

  }

  object Transaction {
    class Ops[S[_]](implicit s0: Transaction :<: S) {
      def commit() = Free.liftF(s0.inj(Commit()))
    }

    object Ops {
      implicit def apply[S[_]](implicit S: Transaction :<: S) = new Ops[S]
    }
  }

  def program[S[_]](implicit
                   logger: Logger.Ops[S],
                   transaction: Transaction.Ops[S]) = {
    for {
      _ <- logger.debug("hi")
      _ <- logger.debug("hi")
      _ <- transaction.commit()
    } yield ()
  }

  type Algebra[A] = Coproduct[Logger, Transaction, A]
  val prog = program[Algebra]

  object LoggerInterpreter extends (Logger ~> Task) {
    override def apply[A](fa: Logger[A]): Task[A] = (fa match {
      case Debug(message) => Task(println(message))
    }).asInstanceOf[Task[A]]
  }

  object TransactionInterpreter extends (Transaction ~> Task) {
    override def apply[A](fa: Transaction[A]): Task[A] = (fa match {
      case Commit() => Task(println("Committing"))
    }).asInstanceOf[Task[A]]
  }

  object EnrichNTOps {
    /*
     :+: taken from Quasar Analytics
     */
    sealed abstract class :+:[F[_], G[_]] {
      type λ[A] = Coproduct[F, G, A]
    }

    implicit class EnrichNT[F[_], H[_]](f: F ~> H) {
      def :+:[G[_]](g: G ~> H): (G :+: F)#λ ~> H = new ((G :+: F)#λ ~> H) {
        def apply[A](fa: (G :+: F)#λ[A]) = fa.run.fold(g, f)
      }
    }
  }

  import EnrichNTOps._

  val translator = LoggerInterpreter :+: TransactionInterpreter

  prog.foldMap(translator).unsafePerformSync

}
