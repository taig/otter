package io.taig.otter

import scala.Tuple as STuple

trait Append[A, B]:
  type Out
  def apply(ab: (A, B)): Out
  def unapply(out: Out): (A, B)

object Append extends Append1:
  type Aux[A, B, C] = Append[A, B] { type Out = C }

  inline def apply[A, B](using self: Append[A, B]): Append.Aux[A, B, self.Out] = self

  inline def apply[A, B, C](using self: Append.Aux[A, B, C]): Append.Aux[A, B, C] = self

  def apply[A, B, C](f: ((A, B)) => C)(g: C => (A, B)): Append.Aux[A, B, C] = new Append[A, B]:
    override type Out = C
    override def apply(ab: (A, B)): Out = f(ab)
    override def unapply(out: C): (A, B) = g(out)

  given [A]: Append.Aux[A, Unit, A] = Append[A, Unit, A](_._1)((_, ()))

  given [A]: Append.Aux[Unit, A, A] = Append[Unit, A, A](_._2)(((), _))

  given [A <: STuple, B]: Append.Aux[A, B, STuple.Append[A, B]] = new Append[A, B]:
    override type Out = STuple.Append[A, B]
    override def apply(ab: (A, B)): Out = ab._1 :* ab._2
    @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
    override def unapply(ab: Out): (A, B) = (ab.init.asInstanceOf[A], ab.last.asInstanceOf[B])

trait Append1:
  given [A, B]: Append.Aux[A, B, (A, B)] = new Append[A, B]:
    override type Out = (A, B)
    override def apply(ab: (A, B)): (A, B) = ab
    override def unapply(ab: (A, B)): (A, B) = ab
