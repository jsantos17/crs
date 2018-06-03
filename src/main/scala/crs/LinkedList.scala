
package crs

import scalaz.{Functor, Cord}
import matryoshka.data._
import matryoshka._
import matryoshka.implicits._
import matryoshka.patterns._

import scalaz.syntax.show._

sealed abstract class LinkedListF[A] extends Product with Serializable

object LinkedListF {
  case class NilF[A]() extends LinkedListF[A]
  case class ConsF[A](e: Int, next: A) extends LinkedListF[A]

  // not the usual List functor
  implicit def functorLinkedListF: Functor[LinkedListF] = new Functor[LinkedListF] {
    def map[A, B](list: LinkedListF[A])(f: A => B): LinkedListF[B] = list match {
      case NilF()         => (NilF(): LinkedListF[B])
      case ConsF(e, next) => ConsF(e, f(next))
    }
  }
}

object LinkedList {
  import LinkedListF._

  def Nil[A]: LinkedList = Fix[LinkedListF](NilF())
  def Cons[A](e: Int, next: LinkedList): LinkedList = Fix(ConsF(e, next))

  type LinkedList = Fix[LinkedListF]

  def showƒ: Algebra[LinkedListF, Cord] = {
    case NilF()         => Cord("]")
    case ConsF(e, next) => Cord(e.toString) ++ Cord(",") ++ next
  }

  def show(list: LinkedList): String =
    (Cord("[") ++ list.cata(showƒ)).shows

  def lengthƒ: Algebra[LinkedListF, Int] = {
    case NilF() => 0
    case ConsF(_, next) => next + 1
  }

  def length(list: LinkedList): Int =
    list.cata(lengthƒ)

  def mapƒ(f: Int => Int): Algebra[LinkedListF, LinkedList] = {
    case NilF()         => Fix(NilF())
    case ConsF(e, next) => Fix(ConsF(f(e), next))
  }

  def map(list: LinkedList)(f: Int => Int): LinkedList =
    list.cata[LinkedList](mapƒ(f))

  def sumƒ: Algebra[LinkedListF, Int] = {
    case NilF() => 0
    case ConsF(e, next) => next + e
  }

  def sum(list: LinkedList): Int =
    list.cata(sumƒ)

  def filterƒ(f: Int => Boolean): Algebra[LinkedListF, LinkedList] = {
    case NilF() => Fix(NilF())
    case ConsF(e, next) if f(e) => Fix(ConsF(e, next))
    case ConsF(_, next) => next
  }

  def filter(predicate: Int => Boolean)(list: LinkedList): LinkedList =
    list.cata(filterƒ(predicate))

  // LinkedListF[(LinkedList, Cord)] => Cord
  def smartShowƒ: GAlgebra[(LinkedList, ?), LinkedListF, Cord] = {
    case NilF() => Cord("]")
    case ConsF(e, (Fix(NilF()), next)) => Cord(e.toString) ++ next
    case ConsF(e, (_, next)) => Cord(e.toString) ++ Cord(",") ++ next
  }

  def smartShow(list: LinkedList): String =
    (Cord("[") ++ list.para(smartShowƒ)).shows

}
