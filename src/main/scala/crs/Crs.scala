package crs

import scalaz._, Scalaz._
import matryoshka.data._
import matryoshka._
import matryoshka.implicits._

object Crs {
  def main(args: Array[String]) = {
    import LinkedList.{Cons, Nil}
    import LinkedListF._

    val separator = "-------------------------------------------------"

    val list = Cons(1, Cons(2, Cons(3, Nil)))

    println("List: ")
    println(LinkedList.show(list))

    // checkpoint_01
    println(separator)
    println("Sums to:")
    println(LinkedList.sum(list))
    println("Length")
    println(LinkedList.length(list))
  }
}
