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

    println("List (with para): ")
    println(LinkedList.smartShow(list))

    println("Doubled list: ")
    println(LinkedList.smartShow(doubledList))
    println("Sums to:")
    println(LinkedList.sum(doubledList))
    println("Length")
    println(LinkedList.length(list))

    println("Filtered, keeping odd")
    val filtered = LinkedList.filter(_ % 2 != 0)(list)

    println(LinkedList.smartShow(filtered))

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

    println("Evaluated tree: ")
    println(Expr.evaluate(tree))

    println("Transformed tree: ")
    println(Expr.show(Expr.transform(tree)))

    println("Cotransformed tree: ")
    println(Expr.show(Expr.cotransform(tree)))

    println(separator)
    println("Annotated")

    println(Expr.showAnn(Expr.annotate(tree)))

    println(separator)

    println("UnaryFn")

    val sampleFn =
      UnaryFn.Multiply(
        UnaryFn.Add(
          UnaryFn.Literal(5),
          UnaryFn.HoleU),
          UnaryFn.Literal(2))

    println(UnaryFn.show(sampleFn))

    println("Spliced UnaryFn")

    val spliced = UnaryFn.splice(10, sampleFn)

    println(Expr.show(spliced))

    println("Spliced and evaluated")
    println(Expr.evaluate(spliced))
  }
}
