package io.taig.otter

import scala.Tuple as STuple

trait Prepend[A, B]:
  type Out
  def apply(ab: (A, B)): Out
  def unapply(out: Out): (A, B)

object Prepend extends Prepend1:
  type Aux[A, B, C] = Prepend[A, B] { type Out = C }

  inline def apply[A, B](using self: Prepend[A, B]): Prepend.Aux[A, B, self.Out] = self

  inline def apply[A, B, C](using self: Prepend.Aux[A, B, C]): Prepend.Aux[A, B, C] = self

  def apply[A, B, C](f: ((A, B)) => C)(g: C => (A, B)): Prepend.Aux[A, B, C] = new Prepend[A, B]:
    override type Out = C
    override def apply(ab: (A, B)): Out = f(ab)
    override def unapply(out: C): (A, B) = g(out)

  given [A] => Prepend.Aux[A, Unit, A] = Prepend[A, Unit, A](_._1)((_, ()))

  given [A] => Prepend.Aux[Unit, A, A] = Prepend[Unit, A, A](_._2)(((), _))

  given [A, B <: STuple] => Prepend.Aux[A, B, A *: B] = new Prepend[A, B]:
    override type Out = A *: B
    override def apply(ab: (A, B)): Out = ab._1 *: ab._2
    override def unapply(ab: Out): (A, B) = (ab.head, ab.tail)

trait Prepend1:
  given [A, B] => Prepend.Aux[A, B, (A, B)] = new Prepend[A, B]:
    override type Out = (A, B)
    override def apply(ab: (A, B)): (A, B) = ab
    override def unapply(ab: (A, B)): (A, B) = ab
