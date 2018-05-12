
package crs

import scalaz.Functor
import matryoshka.data.Fix

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

  type LinkedList = Fix[LinkedListF]
}
