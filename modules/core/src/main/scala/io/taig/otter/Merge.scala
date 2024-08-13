package io.taig.otter

import scala.Tuple as STuple
import scala.deriving.*

trait Merge[A, B]:
  type Out
  def apply(ab: (A, B)): Out
  def unapply(out: Out): (A, B)

object Merge extends Merge1:
  type Aux[A, B, C] = Merge[A, B] { type Out = C }

  inline def apply[A, B](using merge: Merge[A, B]): merge.type = merge

  given [A]: Merge.Aux[A, Unit, A] = instance[A, Unit, A] { case (a, _) => a }(a => (a, ()))

  given [A]: Merge.Aux[Unit, A, A] = instance[Unit, A, A] { case (_, a) => a }(a => ((), a))

trait Merge1 extends Merge2:
  given [A, B <: STuple]: Merge.Aux[A, B, A *: B] =
    instance[A, B, A *: B] { case (a, b) => a *: b } { case a *: b => (a, b) }

  given [A <: STuple, B]: Merge.Aux[A, B, STuple.Append[A, B]] =
    instance[A, B, STuple.Append[A, B]] { case (a, b) => a :* b } { ab =>
      (ab.init.asInstanceOf[A], ab.last.asInstanceOf[B])
    }

trait Merge2:
  def instance[A, B, C](f: ((A, B)) => C)(g: C => (A, B)): Merge.Aux[A, B, C] = new Merge[A, B]:
    override type Out = C
    override def apply(ab: (A, B)): C = f(ab)
    override def unapply(out: C): (A, B) = g(out)

  given [A, B]: Merge.Aux[A, B, (A, B)] = instance[A, B, (A, B)](identity)(identity)
