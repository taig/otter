package io.taig.otter

sealed abstract class Field[+S[_], A] extends Product with Serializable:
  def name: String

  def schema: Reference[S, ?]

  final def imap[T](f: A => T)(g: T => A): Field[S, T] = Field.Modify(self = this, f, g)

  def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Field[T, A]

  final def optional: Field[S, Option[A]] = Field.Optional(self = this)

object Field:
  final case class Modify[S[_], A, B](self: Field[S, A], f: A => B, g: B => A) extends Field[S, B]:
    export self.{name, schema}
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Field[T, B] =
      copy(self = self.mapK[S1, T](fK))

  final case class Optional[S[_], A](self: Field[S, A]) extends Field[S, Option[A]]:
    export self.{name, schema}
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Field[T, Option[A]] =
      copy(self = self.mapK[S1, T](fK))

  final case class Root[S[_], A](name: String, schema: Reference[S, A]) extends Field[S, A]:
    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Field[T, A] =
      copy(schema = schema.mapK[S1, T](fK))
