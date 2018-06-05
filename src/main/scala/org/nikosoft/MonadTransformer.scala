package org.nikosoft

import java.time.{Duration, LocalDateTime}

import scalaz._
import scalaz.concurrent._
import Scalaz._

object MonadTransformer extends App {

  type FuckOff[A] = OptionT[Task, A]
  val t1 = {Thread.sleep(2000); 5}.point[FuckOff]
  val t2 = {Thread.sleep(2000); 10}.point[FuckOff]
  val t3 = {Thread.sleep(2000); 15}.point[FuckOff]

  implicit val n = new Nondeterminism[FuckOff] {
    override def chooseAny[A](head: FuckOff[A], tail: Seq[FuckOff[A]]): FuckOff[(A, Seq[FuckOff[A]])] = ???

    override def point[A](a: => A): FuckOff[A] = ???

    override def bind[A, B](fa: FuckOff[A])(f: A => FuckOff[B]): FuckOff[B] = ???
  }

  val tt = Nondeterminism[FuckOff].gatherUnordered(List(t1, t2, t3))

  val startedAt = LocalDateTime.now
  val result = tt.fold(identity, Nil).unsafePerformSync
  println(result)
  println(s"Total time ${Duration.between(startedAt, LocalDateTime.now)}")

}
