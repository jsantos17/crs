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

    // checkpoint_02
    println(separator)
    val doubledList = LinkedList.map(list)(i => i*2)

    println("Doubled list: ")
    println(LinkedList.show(doubledList))
    println("Sums to:")
    println(LinkedList.sum(doubledList))
    println("Length")
    println(LinkedList.length(list))

    // checkpoint_03
    println(separator)
    println("Filtered, keeping odd")
    val filtered = LinkedList.filter(_ % 2 != 0)(list)

    println(LinkedList.show(filtered))

    // checkpoint_04
    println(separator)
    println("List (with para): ")
    println(LinkedList.smartShow(list))

    // checkpoint_05
    println(separator)
    val tree =
      Expr.Subtract(
        Expr.Multiply(
          Expr.Add(
            Expr.Literal(3),
            Expr.Literal(4)),
          Expr.Literal(6)),
      Expr.Literal(-3))

    println("Tree")
    println(Expr.show(tree))

    // checkpoint_06
    println(separator)
    println("Evaluated tree: ")
    println(Expr.evaluate(tree))

    // checkpoint_07
    println(separator)
    println("Transformed tree: ")
    println(Expr.show(Expr.transform(tree)))
  }
}
