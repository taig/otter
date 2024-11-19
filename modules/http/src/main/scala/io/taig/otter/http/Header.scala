package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.Codec
import io.taig.otter.Data
import io.taig.otter.Merge
import io.taig.otter.collectFirstWithRemainders
import io.taig.otter.Metadata
import org.typelevel.ci.CIString

sealed abstract class Header[A] extends Product, Serializable:
  self =>

  def name: CIString

  def codec: Codec[?, ?]

  final def isNullable: Boolean = codec.isNullable
  final def isRequired: Boolean = codec.isRequired

  def metadata: Metadata

  def modifyMetadata(f: Metadata => Metadata): Header[A]

  def imap[B](f: A => B)(g: B => A): Header[B]

  final def :*[B](header: Header[B])(using merge: Merge[A, B]): Headers[merge.Out] = toHeaders :* header

  final def *:[B](header: Header[B])(using merge: Merge[B, A]): Headers[merge.Out] = header *: toHeaders

  final def toHeaders: Headers[A] = Headers(this)

  def decode(values: Http.Headers): (Http.Headers, Codec.Result[A])

  def encode(a: A): Http.Headers

object Header:
  sealed abstract class Required[A] extends Header[A]:
    override def modifyMetadata(f: Metadata => Metadata): Header.Required[A]
    override def imap[B](f: A => B)(g: B => A): Header.Required[B]
    def optional(default: A): Header[A]
    def optional: Header[Option[A]]

  object Required:
    final case class Primitive[A](name: CIString, codec: Codec[Data.Primitive, A], metadata: Metadata)
        extends Header.Required[A]:
      override def modifyMetadata(f: Metadata => Metadata): Header.Required[A] = copy(metadata = f(metadata))
      override def imap[B](f: A => B)(g: B => A): Header.Required[B] = copy(codec = codec.imap(f)(g))
      override def optional(default: A): Header[A] = Header.Primitive(name, codec = codec.nullable(default), metadata)
      override def optional: Header[Option[A]] = Header.Primitive(name, codec = codec.nullable, metadata)
      override def decode(values: Http.Headers): (Http.Headers, Codec.Result[A]) =
        val (remainders, value) = values.collectFirstWithRemainders { case (`name`, value) => value }
        (remainders, codec.parseNullable(value))
      override def encode(a: A): Http.Headers = Vector((name, codec.print(a)))

    final case class Array[A](
        name: CIString,
        codec: Codec[Data.Array[Data.Primitive], A],
        metadata: Metadata,
        delimiter: Delimiter
    ) extends Header.Required[A]:
      override def modifyMetadata(f: Metadata => Metadata): Header.Required[A] = copy(metadata = f(metadata))
      override def imap[B](f: A => B)(g: B => A): Header.Required[B] = copy(codec = codec.imap(f)(g))
      override def optional(default: A): Header[A] =
        Header.Array(name, codec = codec.nullable(default), metadata, delimiter)
      override def optional: Header[Option[A]] = Header.Array(name, codec = codec.nullable, metadata, delimiter)
      override def decode(values: Http.Headers): (Http.Headers, Codec.Result[A]) =
        val (remainders, value) = values.collectFirstWithRemainders { case (`name`, value) => value }
        (remainders, codec.parseNullableArray(value.map(delimiter.decode)))
      override def encode(a: A): Http.Headers =
        Vector((name, delimiter.encode(codec.printArray(a))))

    final case class Object[A](name: CIString, codec: Codec[Data.Object[Data.Nullable[Data.Primitive]], A], metadata: Metadata)
        extends Header.Required[A]:
      override def modifyMetadata(f: Metadata => Metadata): Header.Required[A] = copy(metadata = f(metadata))
      override def imap[B](f: A => B)(g: B => A): Header.Required[B] = copy(codec = codec.imap(f)(g))
      override def optional(default: A): Header[A] = ??? // Header.Object(name, codec = codec.nullable(default), metadata)
      override def optional: Header[Option[A]] = ??? // Header.Object(name, codec = codec.nullable, metadata)
      override def decode(values: Http.Headers): (Http.Headers, Codec.Result[A]) =
        val (remainders, value) = values.collectFirstWithRemainders { case (`name`, value) => value }
        (remainders, codec.parseNullableObject(value.map(FormData.parse).map(_.toVector)))
      override def encode(a: A): Http.Headers = Vector((name, Printers(FormData(codec.printObject(a)))))

  final case class Primitive[A](name: CIString, codec: Codec[Data.Nullable[Data.Primitive], A], metadata: Metadata)
      extends Header[A]:
    override def modifyMetadata(f: Metadata => Metadata): Header[A] = copy(metadata = f(metadata))
    override def imap[B](f: A => B)(g: B => A): Header[B] = copy(codec = codec.imap(f)(g))
    override def decode(values: Http.Headers): (Http.Headers, Codec.Result[A]) =
      val (remainders, value) = values.collectFirstWithRemainders { case (`name`, value) => value }
      (remainders, codec.parseNullable(value))
    override def encode(a: A): Http.Headers = Vector.from(codec.printNullable(a)).tupleLeft(name)

  final case class Array[A](
      name: CIString,
      codec: Codec[Data.Nullable[Data.Array[Data.Primitive]], A],
      metadata: Metadata,
      delimiter: Delimiter
  ) extends Header[A]:
    override def modifyMetadata(f: Metadata => Metadata): Header[A] = copy(metadata = f(metadata))
    override def imap[B](f: A => B)(g: B => A): Header[B] = copy(codec = codec.imap(f)(g))
    override def decode(values: Http.Headers): (Http.Headers, Codec.Result[A]) =
      val (remainders, value) = values.collectFirstWithRemainders { case (`name`, value) => value }
      (remainders, codec.parseNullableArray(value.map(delimiter.decode)))
    override def encode(a: A): Http.Headers =
      Vector.from(codec.printNullableArray(a).map(delimiter.encode)).tupleLeft(name)

  final case class Object[A](name: CIString, codec: Codec[Data.Nullable[Data.Object[Data.Nullable[Data.Primitive]]], A], metadata: Metadata) extends Header[A]:
    override def modifyMetadata(f: Metadata => Metadata): Header[A] = copy(metadata = f(metadata))
    override def imap[B](f: A => B)(g: B => A): Header[B] = copy(codec = codec.imap(f)(g))
    override def decode(values: Http.Headers): (Http.Headers, Codec.Result[A]) =
      val (remainders, value) = values.collectFirstWithRemainders { case (`name`, value) => value }
      (remainders, codec.parseNullableObject(value.map(FormData.parse).map(_.toVector)))
    override def encode(a: A): Http.Headers =
      codec.printNullableObject(a).map(FormData.apply).map(Printers.apply).toVector.tupleLeft(name)