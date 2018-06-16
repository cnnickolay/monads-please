package org.nikosoft

import scalaz._
import Scalaz._

object LetsTagWithScalaz extends App {

  trait Name
  trait Lastname

  type NameTag = String @@ Name
  type LastnameTag = String @@ Lastname

  val name: NameTag = Tag.of[Name]("niko")
  val lastname: LastnameTag = Tag.of[Lastname]("che")

  def procName(s: NameTag) = println(s"Name is $s")
  def procLastname(s: LastnameTag) = println(s"Lastname is $s")

  procName(name)
  procLastname(lastname)

}
