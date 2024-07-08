package io.taig.otter.http

import cats.data.Chain
import cats.syntax.all.*
import cats.Functor

sealed trait Headers[+F[+_], A] extends Headers.Reader[F, A], Headers.Writer[F, A]:
  override def headers: Chain[Header[F, ?]]
  override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Headers[G, A]

object Headers:
  sealed trait Reader[+F[+_], +A] extends Product, Serializable:
    def headers: Chain[Header.Reader[F, ?]]
    def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Headers.Reader[G, A]

  object Reader:
    final case class Combine[F[+_], A, B](left: Headers.Reader[F, A], right: Headers.Reader[F, B])
        extends Headers.Reader[F, (A, B)]:
      override def headers: Chain[Header.Reader[F, ?]] = left.headers ++ right.headers
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Headers.Reader[G, (A, B)] =
        copy(left = left.translate(fK), right = right.translate(fK))

    final case class One[F[+_], A](header: Header.Reader[F, A]) extends Headers.Reader[F, A]:
      override def headers: Chain[Header.Reader[F, ?]] = Chain.one(header)
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Headers.Reader[G, A] =
        copy(header = header.translate(fK))

  sealed trait Writer[+F[+_], -A] extends Product, Serializable:
    def headers: Chain[Header.Writer[F, ?]]
    def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Headers.Writer[G, A]

  object Writer:
    final case class Combine[F[+_], A, B](left: Headers.Writer[F, A], right: Headers.Writer[F, B])
        extends Headers.Writer[F, (A, B)]:
      override def headers: Chain[Header.Writer[F, ?]] = left.headers ++ right.headers
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Headers.Writer[G, (A, B)] =
        copy(left = left.translate(fK), right = right.translate(fK))

    final case class One[F[+_], A](header: Header.Writer[F, A]) extends Headers.Writer[F, A]:
      override def headers: Chain[Header.Writer[F, ?]] = Chain.one(header)
      override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Headers.Writer[G, A] =
        copy(header = header.translate(fK))

  final case class Combine[F[+_], A, B](left: Headers[F, A], right: Headers[F, B]) extends Headers[F, (A, B)]:
    override def headers: Chain[Header[F, ?]] = left.headers ++ right.headers
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Headers[G, (A, B)] =
      copy(left = left.translate(fK), right = right.translate(fK))

  case object Empty extends Headers[Nothing, Unit]:
    override def headers: Chain[Nothing] = Chain.empty
    override def translate[G[+_]: Functor](fK: [A] => Nothing => G[A]): Headers[G, Unit] = this

  final case class One[F[+_], A](header: Header[F, A]) extends Headers[F, A]:
    override def headers: Chain[Header[F, ?]] = Chain.one(header)
    override def translate[G[+_]: Functor](fK: [A] => F[A] => G[A]): Headers[G, A] =
      copy(header = header.translate(fK))
