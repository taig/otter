package io.taig.otter.http

import cats.data.Chain
import cats.syntax.all.*
import scala.Product as SProduct

sealed trait Headers[A] extends SProduct, Serializable:
  def headers: Chain[Header[?]]

object Headers:
  final case class Combine[A, B](left: Headers[A], right: Headers[B]) extends Headers[(A, B)]:
    override def headers: Chain[Header[?]] = left.headers ++ right.headers

  case object Empty extends Headers[Unit]:
    override def headers: Chain[Nothing] = Chain.empty

  final case class One[A](header: Header[A]) extends Headers[A]:
    override def headers: Chain[Header[?]] = Chain.one(header)
