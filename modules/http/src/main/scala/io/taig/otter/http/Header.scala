package io.taig.otter.http

import cats.syntax.all.*
import org.typelevel.ci.CIString
import io.taig.otter.Metadata
import io.taig.otter.Data
import io.taig.otter.Codec
import scala.Array as SArray
import io.taig.otter.Evidence

sealed abstract class Header[A] extends Product, Serializable:
  self =>

  def name: CIString

  def codec: Codec[?, Data.Primitive | Data.Array[Data.Primitive] | Data.Object[Data.Optional[Data.Primitive]], ?]

  final def isOptional: Boolean = codec.isOptional

  def metadata: Metadata

  def modifyMetadata(f: Metadata => Metadata): Header[A]

  def imap[B](f: A => B)(g: B => A): Header[B]

  def optional: Header[Option[A]]

  final def :*[B](header: Header[B])(using merge: Evidence.Merge[A, B]): Headers[merge.Out] = toHeaders :* header

  final def *:[B](header: Header[B])(using merge: Evidence.Merge[B, A]): Headers[merge.Out] = header *: toHeaders

  final def toHeaders: Headers[A] = Headers(this)

  def decode(header: Option[String]): Codec.Result[A]

  def encode(a: A): Option[String]

object Header:
  final case class Default[A](name: CIString, codec: Codec[Data.Optional, Data.Primitive, A], metadata: Metadata)
      extends Header[A]:
    override def modifyMetadata(f: Metadata => Metadata): Header[A] = copy(metadata = f(metadata))
    override def imap[B](f: A => B)(g: B => A): Header[B] = copy(codec = codec.imap(f)(g))
    override def optional: Header[Option[A]] = copy(codec = codec.optional)
    override def decode(header: Option[String]): Codec.Result[A] = codec.parseOptional(header)
    override def encode(a: A): Option[String] = codec.printOptional(a)

  final case class Array[A](
      name: CIString,
      codec: Codec[Data.Optional, Data.Array[Data.Primitive], A],
      metadata: Metadata
  ) extends Header[A]:
    override def modifyMetadata(f: Metadata => Metadata): Header[A] = copy(metadata = f(metadata))
    override def imap[B](f: A => B)(g: B => A): Header[B] = copy(codec = codec.imap(f)(g))
    override def optional: Header[Option[A]] = copy(codec = codec.optional)
    override def decode(header: Option[String]): Codec.Result[A] =
      codec.parseOptionalArray(header.map(_.split(',').toVector))
    override def encode(a: A): Option[String] = codec.printOptionalArray(a).map(_.mkString(","))

  final case class Object[A](
      name: CIString,
      codec: Codec[Data.Optional, Data.Object[Data.Optional[Data.Primitive]], A],
      metadata: Metadata
  ) extends Header[A]:
    override def modifyMetadata(f: Metadata => Metadata): Header[A] = copy(metadata = f(metadata))
    override def imap[B](f: A => B)(g: B => A): Header[B] = copy(codec = codec.imap(f)(g))
    override def optional: Header[Option[A]] = copy(codec = codec.optional)
    override def decode(header: Option[String]): Codec.Result[A] = codec
      .parseOptionalObject(
        header.map(_.split(',').map(_.split("=", 2)).collect { case SArray(key, value) => (key, value) }.toVector)
      )
    override def encode(a: A): Option[String] = codec
      .printOptionalObject(a)
      .map(_.map { case (key, value) => s"$key=$value" })
      .map(_.mkString(","))
