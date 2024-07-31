package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.Codec
import cats.data.Validated
import io.taig.otter.validation.Violations
import io.taig.otter.filterKeys
import io.taig.otter.Codec.Result

sealed abstract class Headers[A]:
  self =>

  def toVector: Vector[Header[?]]

  final def imap[B](f: A => B)(g: B => A): Headers[B] = new Headers[B]:
    export self.toVector
    override def decode(headers: Http.Headers): Codec.Result[B] = self.decode(headers).map(f)
    override def encode(b: B): Http.Headers = self.encode(g(b))

  final def zip[B](headers: Headers[B]): Headers[(A, B)] = new Headers[(A, B)]:
    override def toVector: Vector[Header[?]] = self.toVector ++ headers.toVector
    override def decode(values: Http.Headers): Codec.Result[(A, B)] =
      val (left, remainders) = values.filterKeys(self.toVector.map(_.name))
      val (right, _) = remainders.filterKeys(headers.toVector.map(_.name))
      (self.decode(left), headers.decode(right)).tupled
    override def encode(ab: (A, B)): Http.Headers = self.encode(ab._1) ++ headers.encode(ab._2)

  def encode(a: A): Http.Headers

  def decode(headers: Http.Headers): Codec.Result[A]

object Headers:
  val Empty: Headers[Unit] = new Headers[Unit]:
    override def toVector: Vector[Header[?]] = Vector.empty
    override def encode(a: Unit): Http.Headers = Vector.empty
    override def decode(headers: Http.Headers): Codec.Result[Unit] = ().valid

  def apply[A](header: Header[A]): Headers[A] = new Headers[A]:
    override def toVector: Vector[Header[?]] = Vector(header)
    override def encode(a: A): Http.Headers = Vector.from(header.encode(a).tupleLeft(header.name))
    override def decode(headers: Http.Headers): Codec.Result[A] =
      header.decode(headers.collectFirst { case (name, value) if name === header.name => value })
