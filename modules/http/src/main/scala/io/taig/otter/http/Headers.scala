package io.taig.otter.http

import cats.data.Chain
import cats.syntax.all.*

sealed trait Headers[+F[+_], +G[+_], A] extends Headers.Reader[F, G, A], Headers.Writer[F, G, A]:
  override def headers: Chain[F[Header[G, ?]]]
  override def translate[H[+_]](fK: [A] => F[A] => H[A]): Headers[H, G, A]

object Headers:
  sealed trait Reader[+F[+_], +G[+_], +A] extends Product, Serializable:
    def headers: Chain[F[Header.Reader[G, ?]]]
    def translate[H[+_]](fK: [A] => F[A] => H[A]): Headers.Reader[H, G, A]

  object Reader:
    final case class Combine[F[+_], G[+_], A, B](left: Headers.Reader[F, G, A], right: Headers.Reader[F, G, B])
        extends Headers.Reader[F, G, (A, B)]:
      override def headers: Chain[F[Header.Reader[G, ?]]] = left.headers ++ right.headers
      override def translate[H[+_]](fK: [A] => F[A] => H[A]): Headers.Reader[H, G, (A, B)] =
        copy(left = left.translate(fK), right = right.translate(fK))

    final case class One[F[+_], G[+_], A](header: F[Header.Reader[G, A]]) extends Headers.Reader[F, G, A]:
      override def headers: Chain[F[Header.Reader[G, ?]]] = Chain.one(header)
      override def translate[H[+_]](fK: [A] => F[A] => H[A]): Headers.Reader[H, G, A] =
        copy(header = fK(header))

  sealed trait Writer[+F[+_], +G[+_], -A] extends Product, Serializable:
    def headers: Chain[F[Header.Writer[G, ?]]]
    def translate[H[+_]](fK: [A] => F[A] => H[A]): Headers.Writer[H, G, A]

  object Writer:
    final case class Combine[F[+_], G[+_], A, B](left: Headers.Writer[F, G, A], right: Headers.Writer[F, G, B])
        extends Headers.Writer[F, G, (A, B)]:
      override def headers: Chain[F[Header.Writer[G, ?]]] = left.headers ++ right.headers
      override def translate[H[+_]](fK: [A] => F[A] => H[A]): Headers.Writer[H, G, (A, B)] =
        copy(left = left.translate(fK), right = right.translate(fK))

    final case class One[F[+_], G[+_], A](header: F[Header.Writer[G, A]]) extends Headers.Writer[F, G, A]:
      override def headers: Chain[F[Header.Writer[G, ?]]] = Chain.one(header)
      override def translate[H[+_]](fK: [A] => F[A] => H[A]): Headers.Writer[H, G, A] = copy(header = fK(header))

  final case class Combine[F[+_], G[+_], A, B](left: Headers[F, G, A], right: Headers[F, G, B])
      extends Headers[F, G, (A, B)]:
    override def headers: Chain[F[Header[G, ?]]] = left.headers ++ right.headers
    override def translate[H[+_]](fK: [A] => F[A] => H[A]): Headers[H, G, (A, B)] =
      copy(left = left.translate(fK), right = right.translate(fK))

  case object Empty extends Headers[Nothing, Nothing, Unit]:
    override def headers: Chain[Nothing] = Chain.empty
    override def translate[H[+_]](fK: [A] => Nothing => H[A]): Headers[H, Nothing, Unit] = this

  final case class One[F[+_], G[+_], A](header: F[Header[G, A]]) extends Headers[F, G, A]:
    override def headers: Chain[F[Header[G, ?]]] = Chain.one(header)
    override def translate[H[+_]](fK: [A] => F[A] => H[A]): Headers[H, G, A] = copy(header = fK(header))
