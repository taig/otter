package io.taig.otter

import cats.Invariant
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import io.taig.enumeration.ext.Mapping
import io.taig.otter.codec.Encoder
import io.taig.otter.operation.EnumerationOperation

sealed abstract class Enumeration[+S[_], A] extends Product with Serializable:
  def schema: Reference[S, ?]

  def values: NonEmptyChain[A]

  def encode[T](encoder: Encoder[S, T]): NonEmptyList[T]

  final def imap[T](f: A => T)(g: T => A): Enumeration[S, T] = Enumeration.Modify(self = this, f, g)

  def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Enumeration[T, A]

object Enumeration:
  final case class Modify[S[_], A, B](self: Enumeration[S, A], f: A => B, g: B => A) extends Enumeration[S, B]:
    export self.{encode, schema}

    override def values: NonEmptyChain[B] = self.values.map(f)

    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Enumeration[T, B] =
      copy(self = self.mapK[S1, T](fK))

  final case class Root[S[_], A, B](schema: Reference[S, A], mapping: Mapping[B, A]) extends Enumeration[S, B]:
    override def values: NonEmptyChain[B] = NonEmptyChain.fromNonEmptyList(mapping.values)

    override def encode[T](encoder: Encoder[S, T]): NonEmptyList[T] =
      mapping.values.map(mapping.apply).map(encoder.encode(schema = schema.value, _))

    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Enumeration[T, B] =
      copy(schema = schema.mapK[S1, T](fK))

  given invariant[S[_]]: Invariant[Enumeration[S, *]] with
    override def imap[A, B](fa: Enumeration[S, A])(f: A => B)(g: B => A): Enumeration[S, B] = fa.imap(f)(g)

  given operation[S[_]]: EnumerationOperation[Enumeration[S, *], S] with
    override def enumeration[A, B](schema: => S[A], mapping: Mapping[B, A]): Enumeration[S, B] =
      Root(schema = Reference.later(schema), mapping)

    override def encode[A, T](self: Enumeration[S, A])(encoder: Encoder[S, T]): NonEmptyList[T] =
      self.encode(encoder)
