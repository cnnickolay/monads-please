package org.nikosoft

import java.util.concurrent.{ThreadFactory, Executors}

import scalaz._
import scalaz.concurrent._

/**
  * Created by Nikolay Cherkezishvili on 29/05/2018
  */
object TasksAndConcurrency extends App {

  val t1 = Task {
    println(s">>> ${Thread.currentThread().getName}")
    Thread.sleep(2000)
    println("<<<")
    10
  }

  val t2 = Task {
    println(s">>> ${Thread.currentThread().getName}")
    Thread.sleep(2000)
    println("<<<")
    20
  }

/*
  implicit val n = new Nondeterminism[Task] {
    override def chooseAny[A](head: Task[A], tail: Seq[Task[A]]): Task[(A, Seq[Task[A]])] = new Task(head.get.map(_.map((_, tail))))

    override def point[A](a: => A): Task[A] = Task(a)

    override def bind[A, B](fa: Task[A])(f: (A) => Task[B]): Task[B] = fa.flatMap(f)
  }
*/

//  val task: Task[List[Int]] = Nondeterminism[Task].gatherUnordered(Seq(t1, t2))

//  Task.fork(task).unsafePerformSync

//  (t1 |@| t2)

}
