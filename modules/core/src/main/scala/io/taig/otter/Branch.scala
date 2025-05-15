package io.taig.otter

import io.taig.otter.Metadata
import java.lang.String as JString
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import scala.Boolean as SBoolean
import scala.Double as SDouble
import scala.Float as SFloat
import scala.Int as SInt
import scala.Long as SLong

sealed abstract class Branch[+S[_], +T[_], A] extends Product with Serializable:
  def key: Reference.Constant[S, ?]
  def value: Reference[T, ?]

  def metadata: Metadata
  def modifyMetadata(f: Metadata => Metadata): Branch[S, T, A]

  final def imap[B](f: A => B)(g: B => A): Branch[S, T, B] = Branch.Modify(self = this, f, g)

  def mapK[T1[a] >: T[a], U[_]](fK: [A] => T1[A] => U[A]): Branch[S, U, A]

object Branch:
  final private[otter] case class Modify[S[_], T[_], A, B](self: Branch[S, T, A], f: A => B, g: B => A)
      extends Branch[S, T, B]:
    export self.{key, metadata, value}
    override def modifyMetadata(f: Metadata => Metadata): Branch[S, T, B] = copy(self = self.modifyMetadata(f))
    override def mapK[T1[a] >: T[a], U[_]](fK: [A] => T1[A] => U[A]): Branch[S, U, B] =
      copy(self = self.mapK[T1, U](fK))

  final private[otter] case class Root[S[_], T[_], A, B](
      key: Reference.Constant[S, A],
      value: Reference[T, B],
      metadata: Metadata
  ) extends Branch[S, T, B]:
    override def modifyMetadata(f: Metadata => Metadata): Branch[S, T, B] = copy(metadata = f(metadata))
    override def mapK[T1[a] >: T[a], U[_]](fK: [A] => T1[A] => U[A]): Branch[S, U, B] =
      copy(value = value.mapK[T1, U](fK))

  given [Key[_], Value[_]]: Schema.Branch[Branch[Key, Value, *], Key, Value] with
    override def branch[A, B](name: A, key: => Key[A], value: => Value[B]): Branch[Key, Value, B] =
      Root(
        key = Reference.Constant(self = Reference.later(key), value = name),
        value = Reference.later(value),
        metadata = Metadata.Empty
      )

    extension [A](self: Branch[Key, Value, A])
      override def key: Reference.Constant[Key, ?] = self.key
      override def value: Reference[Value, ?] = self.value
      override def imap[B](f: A => B)(g: B => A): Branch[Key, Value, B] = self.imap(f)(g)
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Branch[Key, Value, A] = self.modifyMetadata(f)
