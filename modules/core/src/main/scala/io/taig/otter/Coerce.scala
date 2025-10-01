package io.taig.otter

sealed abstract class Coerce[+S[_], A] extends Product with Serializable:
  final def imap[T](f: A => T)(g: T => A): Coerce[S, T] = Coerce.Modify(self = this, f, g)

  def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Coerce[T, A]

object Coerce:
  final case class Modify[S[_], A, B](self: Coerce[S, A], f: A => B, g: B => A) extends Coerce[S, B]:
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Coerce[T, B] =
      copy(self = self.mapK[S1, T](fK))

  final case class Root[S[_], A](schema: Reference[S, A]) extends Coerce[S, A]:
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Coerce[T, A] =
      copy(schema = schema.mapK[S1, T](fK))
