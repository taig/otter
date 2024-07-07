package io.taig.otter.http

import cats.data.Chain
import cats.syntax.all.*

sealed trait Headers[+F[+_], A] extends Headers.Reader[F, A], Headers.Writer[F, A]:
  override def headers: Chain[Header[F, ?]]

object Headers:
  sealed trait Reader[+F[+_], +A]:
    def headers: Chain[Header.Reader[F, ?]]

  object Reader:
    final case class Combine[F[+_], A, B](left: Headers.Reader[F, A], right: Headers.Reader[F, B])
        extends Headers.Reader[F, (A, B)]:
      override def headers: Chain[Header.Reader[F, ?]] = left.headers ++ right.headers

    final case class One[F[+_], A](header: Header.Reader[F, A]) extends Headers.Reader[F, A]:
      override def headers: Chain[Header.Reader[F, ?]] = Chain.one(header)

  sealed trait Writer[+F[+_], -A]:
    def headers: Chain[Header.Writer[F, ?]]

  object Writer:
    final case class Combine[F[+_], A, B](left: Headers.Writer[F, A], right: Headers.Writer[F, B])
        extends Headers.Writer[F, (A, B)]:
      override def headers: Chain[Header.Writer[F, ?]] = left.headers ++ right.headers

    final case class One[F[+_], A](header: Header.Writer[F, A]) extends Headers.Writer[F, A]:
      override def headers: Chain[Header.Writer[F, ?]] = Chain.one(header)

  final case class Combine[F[+_], A, B](left: Headers[F, A], right: Headers[F, B]) extends Headers[F, (A, B)]:
    override def headers: Chain[Header[F, ?]] = left.headers ++ right.headers

  case object Empty extends Headers[Nothing, Unit]:
    override def headers: Chain[Nothing] = Chain.empty

  final case class One[F[+_], A](header: Header[F, A]) extends Headers[F, A]:
    override def headers: Chain[Header[F, ?]] = Chain.one(header)
