package org.nikosoft

import java.util.concurrent.Executors

import scalaz._
import Scalaz._
import scalaz.concurrent._

/**
  * Created by Nikolay Cherkezishvili on 29/05/2018
  */
object TasksAndStateConcurrency extends App {

  val t1 = StateT[Task, Int, Int](i => Task {
    println(s">>> ${Thread.currentThread().getName}")
    Thread.sleep(2000)
    println("<<<")
    (10, i + 10)
  })

  val t2 = StateT[Task, Int, Int](i => Task {
    println(s">>> ${Thread.currentThread().getName}")
    Thread.sleep(2000)
    println("<<<")
    (10, i + 10)
  })

  type IntState[A] = StateT[Task, Int, A]

  implicit val n = new Nondeterminism[IntState] {
    override def chooseAny[A](head: IntState[A], tail: Seq[IntState[A]]): IntState[(A, Seq[IntState[A]])] = for {
      a <- head
    } yield (a, tail)

    override def point[A](a: => A): IntState[A] = StateT[Task, Int, A](i => Task((i, a)))

    override def bind[A, B](fa: IntState[A])(f: (A) => IntState[B]): IntState[B] = for {
      a <- fa
      b <- f(a)
    } yield b
  }

  val result = Nondeterminism[IntState].gatherUnordered(Seq(t1, t2))
  val task: Task[(Int, List[Int])] = result.run(100)

  Task.fork(task)(Executors.newFixedThreadPool(5)).unsafePerformSync

}
