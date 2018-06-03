package org.nikosoft

import scalaz._
import scalaz.concurrent.Task

import scala.language.higherKinds

object FreeFreeMonad extends App {

  trait Logger[T]
  case class Debug(message: String) extends Logger[String]

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

  trait Wrapper[A]
  case class ApplicativeWrapper[S[_], A](app: Free[S, A]) extends Wrapper[A]

  object Transaction {
    class Ops[S[_]](implicit s0: Transaction :<: S) {
      def commit() = Free.liftF(s0.inj(Commit()))
    }

    object Ops {
      implicit def apply[S[_]](implicit S: Transaction :<: S) = new Ops[S]
    }
  }

  object Wrapper {
    class Ops[S[_]](implicit s0: Wrapper :<: S) {
      def applicativeWrapper[S[_], A](app: Free[S, A]) = Free.liftF(s0.inj(ApplicativeWrapper[S, A](app)))
    }

    object Ops {
      implicit def apply[S[_]](implicit S: Wrapper :<: S) = new Ops[S]
    }
  }

  def program[S[_]](implicit
                   logger: Logger.Ops[S],
                   wrapper: Wrapper.Ops[S]) = {

    for {
      _ <- logger.debug("monadic")
      _ <- wrapper.applicativeWrapper(logger.debug("monadic"))
    } yield ()
  }

  type Algebra[A] = Coproduct[Logger, Wrapper, A]
  val prog = program[Algebra]

  object LoggerInterpreter extends (Logger ~> Task) {
    override def apply[A](fa: Logger[A]): Task[A] = (fa match {
      case Debug(message) => Task {
        println(">>>")
        println(message)
        Thread.sleep(500)
        println("<<<")
        message + "_zzz"
      }
    }).asInstanceOf[Task[A]]
  }

  object TransactionInterpreter extends (Transaction ~> Task) {
    override def apply[A](fa: Transaction[A]): Task[A] = (fa match {
      case Commit() => Task(println("Committing"))
    }).asInstanceOf[Task[A]]
  }

  object WrapperInterpreter extends (Wrapper ~> Task) {
    override def apply[A](fa: Wrapper[A]): Task[A] = fa match {
      case ApplicativeWrapper(free) => free.asInstanceOf[Free[Algebra, A]].foldMap(translator)
    }
  }

  object EnrichNTOps {
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

  val translator = LoggerInterpreter :+: WrapperInterpreter

  prog.foldMap(translator).unsafePerformSync

}
