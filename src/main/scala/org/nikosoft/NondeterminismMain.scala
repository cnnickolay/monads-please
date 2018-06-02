package org.nikosoft

import scalaz._
import Scalaz._
import scalaz.concurrent._
import scalaz.Nondeterminism, scalaz.concurrent.Task


object NondeterminismMain extends App {

  val t1 = Task.delay {
    println("first started")
    Thread.sleep(5000)
    "hi"
  }

  val t2 = Task.delay {
    println("second started")
    Thread.sleep(5000)
    "there"
  }

//  private val tasks = List(t1, t2)
//  val r = Nondeterminism[Task].gatherUnordered(tasks).unsafePerformSync
//  val r1 = tasks.sequenceU.unsafePerformSync

  val r = (t1 |@| t2) ((x, a) => List(x, a))

  println(r.unsafePerformSync)

}
