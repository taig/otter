package io.taig.otter.http

import cats.data.Chain
import cats.syntax.all.*

sealed trait Headers[A] extends Headers.Reader[A], Headers.Writer[A]:
  override def headers: Chain[Header[?]]

object Headers:
  sealed trait Reader[+A] extends Product, Serializable:
    def headers: Chain[Header.Reader[?]]

  object Reader:
    final case class Combine[A, B](left: Headers.Reader[A], right: Headers.Reader[B]) extends Headers.Reader[(A, B)]:
      override def headers: Chain[Header.Reader[?]] = left.headers ++ right.headers

    final case class One[A](header: Header.Reader[A]) extends Headers.Reader[A]:
      override def headers: Chain[Header.Reader[?]] = Chain.one(header)

  sealed trait Writer[-A] extends Product, Serializable:
    def headers: Chain[Header.Writer[?]]

  object Writer:
    final case class Combine[A, B](left: Headers.Writer[A], right: Headers.Writer[B]) extends Headers.Writer[(A, B)]:
      override def headers: Chain[Header.Writer[?]] = left.headers ++ right.headers

    final case class One[A](header: Header.Writer[A]) extends Headers.Writer[A]:
      override def headers: Chain[Header.Writer[?]] = Chain.one(header)

  final case class Combine[A, B](left: Headers[A], right: Headers[B]) extends Headers[(A, B)]:
    override def headers: Chain[Header[?]] = left.headers ++ right.headers

  case object Empty extends Headers[Unit]:
    override def headers: Chain[Nothing] = Chain.empty

  final case class One[A](header: Header[A]) extends Headers[A]:
    override def headers: Chain[Header[?]] = Chain.one(header)
