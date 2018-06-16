package org.nikosoft

import shapeless._
import shapeless.tag.@@

/**
  * shapeless do tags much better than scalaz defo
  */
object LetsTagWithShapeless extends App {

  trait Name
  trait Lastname

  type NameTag = String @@ Name
  type LastnameTag = String @@ Lastname

  val name: NameTag = tag[Name]("niko")
  val lastname: LastnameTag = tag[Lastname]("che")

  def procName(s: NameTag) = println(s"Name is $s")
  def procLastname(s: LastnameTag) = println(s"Lastname is $s")
  def procString(s: String) = println(s"Generic string is $s")

  procName(name)
  procLastname(lastname)
  procString(name)

}
