package org.nikosoft

import scalaz._
import Scalaz._

object StateMonadPleaseSaveMeFromVariables extends App {

  case class Memory(i: Int = 0)

  val state = State[Memory, Unit](memory => (memory.copy(i = memory.i + 1), Unit))
  val getValue = State[Memory, Int](memory => (memory, memory.i))

  val result = for {
    _ <- state
    _ <- state
    _ <- state
    _ <- state
    value <- getValue
  } yield value

  println(result.eval(Memory()))

}
