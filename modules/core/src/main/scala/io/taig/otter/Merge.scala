package io.taig.otter

import scala.Tuple as STuple

trait Merge[A, B]:
  type Out
  def apply(ab: (A, B)): Out
  def unapply(out: Out): (A, B)

object Merge extends Merge1:
  type Aux[A, B, C] = Merge[A, B] { type Out = C }

  inline def apply[A, B](using merge: Merge[A, B]): merge.type = merge

  given [A]: Merge.Aux[A, Unit, A] = new Merge[A, Unit]:
    override type Out = A
    override def apply(ab: (A, Unit)): Out = ab._1
    override def unapply(a: A): (A, Unit) = (a, ())

  given [A]: Merge.Aux[Unit, A, A] = new Merge[Unit, A]:
    override type Out = A
    override def apply(ab: (Unit, A)): Out = ab._2
    override def unapply(a: A): (Unit, A) = ((), a)

trait Merge1 extends Merge2:
  given [A, B <: STuple]: Merge.Aux[A, B, A *: B] = new Merge[A, B]:
    override type Out = A *: B
    override def apply(ab: (A, B)): Out = ab._1 *: ab._2
    override def unapply(ab: A *: B): (A, B) = (ab.head, ab.tail)

  given [A <: STuple, B]: Merge.Aux[A, B, STuple.Append[A, B]] = new Merge[A, B]:
    override type Out = STuple.Append[A, B]
    override def apply(ab: (A, B)): Out = ab._1 :* ab._2
    override def unapply(ab: Out): (A, B) = (ab.init.asInstanceOf[A], ab.last.asInstanceOf[B])

trait Merge2:
  given [A, B]: Merge.Aux[A, B, (A, B)] = new Merge[A, B]:
    override type Out = (A, B)
    override def apply(ab: (A, B)): (A, B) = ab
    override def unapply(ab: (A, B)): (A, B) = ab
