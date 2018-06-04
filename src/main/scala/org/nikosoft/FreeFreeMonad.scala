package org.nikosoft

import scalaz._
import Scalaz._
import org.nikosoft.IotaHelpMeJoinAlgebrasPlease.{interpreter, service}
import scalaz.concurrent.Task

import scala.language.higherKinds

/**
  * We need to run some monads in parallel for better utilization of resources
  */
object FreeFreeMonad extends App {

  type MyRes[A] = OptionT[Task, A]

  trait Logger[T]
  case class Debug(message: String) extends Logger[String]

  trait Transaction[T]
  case class Commit() extends Transaction[Unit]

  object Logger {
    class Ops[S[_]](implicit s0: Logger :<: S) {
      def debug(message: String) = Free.liftF(s0.inj(Debug(message)))
      def debugAp(message: String) = FreeAp.lift(s0.inj(Debug(message)))
    }
    object Ops {
      implicit def apply[S[_]](implicit S: Logger :<: S) = new Ops[S]
    }
  }

  trait Wrapper[A]
  case class SingleMonadWrapper[S[_], A](monad: Free[S, A]) extends Wrapper[A]
  case class SequenceMonadWrapper[S[_], A](monads: Seq[Free[S, A]]) extends Wrapper[List[A]]
  case class ApplicativeWrapper[S[_], A](applicative: FreeAp[S, A]) extends Wrapper[A]

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
      implicit class SingleMonadWrapperOps[S[_], A](monad: Free[S, A]) {
        def liftPar = singleMonadWrapper(monad)
      }
      def singleMonadWrapper[S[_], A](monad: Free[S, A]) = Free.liftF(s0.inj(SingleMonadWrapper[S, A](monad)))

      implicit class SequenceMonadWrapperOps[S[_], A](monads: Seq[Free[S, A]]) {
        def liftPar = sequenceMonadWrapper(monads)
      }
      def sequenceMonadWrapper[S[_], A](monads: Seq[Free[S, A]]) = Free.liftF(s0.inj(SequenceMonadWrapper[S, A](monads)))

      implicit class ApplicativeWrapperOps[S[_], A](applicative: FreeAp[S, A]) {
        def liftParApp = applicativeWrapper(applicative)
      }
      def applicativeWrapper[S[_], A](applicative: FreeAp[S, A]) = Free.liftF(s0.inj(ApplicativeWrapper[S, A](applicative)))
    }

    object Ops {
      implicit def apply[S[_]](implicit S: Wrapper :<: S) = new Ops[S]
    }
  }

  def program[S[_]](implicit
                    logger: Logger.Ops[S],
                    wrapper: Wrapper.Ops[S],
                    transaction: Transaction.Ops[S]) = {

    import logger._
    import wrapper._
    import transaction._

    for {
      _ <-         debug("monadic")
      _ <-         debug("hi").liftPar
      result <- Seq(
                   debug("all"),
                   debug("functions"),
                   debug("should"),
                   debug("run"),
                   debug("asynchronously")).liftPar
      _ <-         commit()
    } yield ""
  }

  type TransactionAndWrapper[A] = Coproduct[Transaction, Wrapper, A]
  type Algebra[A] = Coproduct[Logger, TransactionAndWrapper, A]
  val prog = program[Algebra]

  object LoggerInterpreter extends (Logger ~> MyRes) {
    override def apply[A](fa: Logger[A]): MyRes[A] = fa match {
      case Debug(message) => OptionT[Task, A] (Task{
        println(s">>> ${Thread.currentThread().getName}")
        println(message)
        Thread.sleep(500)
        println("<<<")
        Option(message + "_zzz").asInstanceOf[Option[A]]
      })
    }
  }

  object TransactionInterpreter extends (Transaction ~> MyRes) {
    override def apply[A](fa: Transaction[A]): MyRes[A] = fa match {
      case Commit() => OptionT[Task, A](Task{
        println("Committing")
        Some(())
      })
    }
  }

  object WrapperInterpreter extends (Wrapper ~> MyRes) {
    override def apply[A](fa: Wrapper[A]): MyRes[A] = fa match {
      case SingleMonadWrapper(monad) => monad.asInstanceOf[Free[Algebra, A]].foldMap(translator)
      case SequenceMonadWrapper(monads) => OptionT[Task, A] {
        val task = Nondeterminism[Task].gatherUnordered(
          monads.asInstanceOf[Seq[Free[Algebra, A]]].map(_.foldMap(translator).run)
        )

        val resultTask = task.map(_.foldLeft(List.empty[A]) {
          case (list, Some(elt)) => elt +: list
        }).map(Option(_)).asInstanceOf[Task[Option[A]]]
        resultTask
      }
      case ApplicativeWrapper(applicative) =>
        applicative.asInstanceOf[FreeAp[Algebra, A]].foldMap(translator)
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

  val transactionAndWrapperTranslator: TransactionAndWrapper ~> MyRes = TransactionInterpreter :+: WrapperInterpreter
  val translator: Algebra ~> MyRes = LoggerInterpreter :+: transactionAndWrapperTranslator

  val result = prog.foldMap(translator).run.unsafePerformSync
  println(s"Here we are: $result")

}
