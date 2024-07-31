package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.findWithRemainders
import org.typelevel.ci.CIString
import io.taig.otter.validation.Violations
import io.taig.otter.Data
import io.taig.otter.Codec
import io.taig.otter.Collection
import io.taig.otter.Metadata

sealed abstract class Header[A]:
  def name: CIString

  def codec: Codec[?, Data.Primitive | Data.Array[Data.Primitive] | Data.Object[Data.Primitive], ?]

  def metadata: Metadata

  def modifyMetadata(f: Metadata => Metadata): Header[A] = ???

  final def imap[B](f: A => B)(g: B => A): Header[B] = ???

  def decode(headers: Http.Headers): Codec.Result[(Http.Headers, A)]

  def encode(a: A): Option[(CIString, String)]

object Header:
  def apply[A](name: CIString, codec: Codec[?, Data.Primitive, A]): Header[A] =
    val _name = name
    val _codec = codec

    new Header[A]:
      override def name: CIString = _name
      override def codec: Codec[?, Data.Primitive, A] = _codec
      override def metadata: Metadata = Metadata.Empty
      override def decode(headers: Http.Headers): Codec.Result[(Http.Headers, A)] =
        val (value, remainders) = headers.findWithRemainders { case (`_name`, value) => value }
        codec.parseOptional(value).tupleLeft(remainders)
      override def encode(a: A): Option[(CIString, String)] = codec.printOptional(a).tupleLeft(name)

  def array[A](name: CIString, codec: Codec[?, Data.Array[Data.Primitive], A]): Header[A] =
    val _name = name
    val _codec = codec

    new Header[A]:
      override def name: CIString = _name
      override def codec: Codec[?, Data.Array[Data.Primitive], A] = _codec
      override def metadata: Metadata = Metadata.Empty
      override def decode(headers: Http.Headers): Codec.Result[(Http.Headers, A)] =
        val (value, remainders) = headers.findWithRemainders { case (`_name`, value) => value.split(',').toVector }
        codec.parseOptionalArray(value).tupleLeft(remainders)
      override def encode(a: A): Option[(CIString, String)] =
        codec.printOptionalArray(a).map(_.mkString(",")).tupleLeft(name)

  def obj[A](name: CIString, codec: Codec[?, Data.Object[Data.Primitive], A]): Header[A] =
    val _name = name
    val _codec = codec

    new Header[A]:
      override def name: CIString = _name
      override def codec: Codec[?, Data.Object[Data.Primitive], A] = _codec
      override def metadata: Metadata = Metadata.Empty
      override def decode(headers: Http.Headers): Codec.Result[(Http.Headers, A)] =
        val (value, remainders) = headers.findWithRemainders { case (`_name`, value) =>
          value.split(',').map(_.split("=", 2)).collect { case Array(key, value) => (key, value) }.toVector
        }

        codec.parseOptionalObject(value).tupleLeft(remainders)
      override def encode(a: A): Option[(CIString, String)] = codec
        .printOptionalObject(a)
        .map(_.map { case (key, value) => s"$key=$value" })
        .map(_.mkString(","))
        .tupleLeft(name)
