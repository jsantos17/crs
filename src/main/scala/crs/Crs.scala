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
    val doubledList = LinkedList.map(list)(i => i*2)

    println("List: ")
    println(LinkedList.show(list))
    println("Sums to:")
    println(LinkedList.sum(list))
    println("Length")
    println(LinkedList.length(list))

    println("Doubled list: ")
    println(LinkedList.show(doubledList))
    println("Sums to:")
    println(LinkedList.sum(doubledList))
    println("Length")
    println(LinkedList.length(list))

    println(separator)

    val tree =
      Expr.Subtract(
        Expr.Multiply(
          Expr.Add(
            Expr.Literal(3),
            Expr.Literal(4)),
          Expr.Literal(6)),
      Expr.Literal(-3))

    println("Tree: ")
    println(Expr.evaluate(tree))

    println("Transformed tree: ")
    println(Expr.evaluate(Expr.transform(tree)))
  }
}
