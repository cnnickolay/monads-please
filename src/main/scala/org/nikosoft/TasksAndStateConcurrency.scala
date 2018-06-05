package org.nikosoft

import java.util.concurrent.Executors

import scalaz._
import Scalaz._
import scalaz.concurrent._

/**
  * Just trying to figure out how Nondeterminism works
  *
  * Created by Nikolai Cherkezishvili on 29/05/2018
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
    val F = Nondeterminism[Future]
    override def chooseAny[A](h: IntState[A], t: Seq[IntState[A]]): IntState[(A, Seq[IntState[A]])] =
      h.map((_, t))

    override def point[A](a: => A): IntState[A] = StateT[Task, Int, A](i => Task((i, a)))

    override def bind[A, B](fa: IntState[A])(f: (A) => IntState[B]): IntState[B] = for {
      a <- fa
      b <- f(a)
    } yield b
  }

  val result = Nondeterminism[IntState].nmap2(t1, t2)(_ + _)
  val task = result.run(100)

  Task.fork(task)(Executors.newFixedThreadPool(5)).unsafePerformSync

}
