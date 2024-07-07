package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter as Base
import io.taig.otter.Value
import org.typelevel.ci.CIString
import cats.Functor

sealed trait Header[+F[+_], A] extends Header.Reader[F, A], Header.Writer[F, A]:
  override def schema: F[Base.Value[F, ?, ?]]
  final def imap[B](f: A => B)(g: B => A): Header[F, B] = Header.Transform(this, f, g)
  override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Header[G, A]

object Header:
  sealed trait Reader[+F[+_], +A] extends Product, Serializable:
    final def map[B](f: A => B): Header.Reader[F, B] = Reader.Transform(this, f)
    def name: CIString
    def schema: F[Base.Value.Reader[F, ?, ?]]
    def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Header.Reader[G, A]

  object Reader:
    final case class Root[F[+_], A](name: CIString, schema: F[Base.Value.Reader[F, ?, A]]) extends Header.Reader[F, A]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Header.Reader[G, A] = 
        copy(schema = fK(schema).map(_.translate(fK)))

    final case class Transform[F[+_], A, B](self: Header.Reader[F, A], f: A => B) extends Header.Reader[F, B]:
      export self.{name, schema}
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Header.Reader[G, B] = 
        copy(self = self.translate(fK))

  sealed trait Writer[+F[+_], -A] extends Product, Serializable:
    final def contramap[B](f: B => A): Header.Writer[F, B] = Writer.Transform(this, f)
    def name: CIString
    def schema: F[Base.Value.Writer[F, ?, ?]]
    def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Header.Writer[G, A]

  object Writer:
    final case class Root[+F[+_], A](name: CIString, schema: F[Base.Value.Writer[F, ?, A]]) extends Header.Writer[F, A]:
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Header.Writer[G, A] = 
        copy(schema = fK(schema).map(_.translate(fK)))

    final case class Transform[F[+_], A, B](self: Header.Writer[F, A], f: B => A) extends Header.Writer[F, B]:
      export self.{name, schema}
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Header.Writer[G, B] = 
        copy(self = self.translate(fK))

  final case class Root[+F[+_], A](name: CIString, schema: F[Base.Value[F, ?, A]]) extends Header[F, A]:
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Header[G, A] = 
        copy(schema = fK(schema).map(_.translate(fK)))

  final case class Transform[F[+_], A, B](self: Header[F, A], f: A => B, g: B => A) extends Header[F, B]:
    export self.{name, schema}
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Header[G, B] = 
        copy(self = self.translate(fK))
