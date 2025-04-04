package io.taig.otter

import scala.Tuple as STuple
import scala.compiletime.*

trait Merge[A, B]:
  type Out
  def apply(ab: (A, B)): Out
  def unapply(out: Out): (A, B)

object Merge extends Merge1:
  type Aux[A, B, C] = Merge[A, B] { type Out = C }

  inline def apply[A, B](using merge: Merge[A, B]): Merge.Aux[A, B, merge.Out] = merge

  inline def apply[A, B, C](using merge: Merge.Aux[A, B, C]): Merge.Aux[A, B, C] = merge

  def instance[A, B, C](f: ((A, B)) => C)(g: C => (A, B)): Merge.Aux[A, B, C] = new Merge[A, B]:
    override type Out = C
    override def apply(ab: (A, B)): Out = f(ab)
    override def unapply(out: C): (A, B) = g(out)

  given [A]: Merge.Aux[A, Unit, A] = instance[A, Unit, A](_._1)((_, ()))

  given [A]: Merge.Aux[Unit, A, A] = instance[Unit, A, A](_._2)(((), _))

  inline given [A <: STuple, B <: STuple]: Merge.Aux[A, B, STuple.Concat[A, B]] =
    val size = erasedValue[STuple.Size[A]]
    instance[A, B, STuple.Concat[A, B]] { case (a, b) => a ++ b } { ab =>
      val (a, b) = ab.splitAt(size)
      (a.asInstanceOf[A], b.asInstanceOf[B])
    }

trait Merge1 extends Merge2:
  given [A, B <: STuple]: Merge.Aux[A, B, A *: B] = new Merge[A, B]:
    override type Out = A *: B
    override def apply(ab: (A, B)): Out = ab._1 *: ab._2
    override def unapply(ab: A *: B): (A, B) = (ab.head, ab.tail)

  given [A <: STuple, B]: Merge.Aux[A, B, STuple.Append[A, B]] = new Merge[A, B]:
    override type Out = STuple.Append[A, B]
    override def apply(ab: (A, B)): Out = ab._1 :* ab._2
    @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
    override def unapply(ab: Out): (A, B) = (ab.init.asInstanceOf[A], ab.last.asInstanceOf[B])

trait Merge2:
  given [A, B]: Merge.Aux[A, B, (A, B)] = new Merge[A, B]:
    override type Out = (A, B)
    override def apply(ab: (A, B)): (A, B) = ab
    override def unapply(ab: (A, B)): (A, B) = ab
