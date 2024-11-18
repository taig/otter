package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.Codec
import io.taig.otter.Data
import io.taig.otter.Merge
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

  def optional: Header[Option[A]]

  final def :*[B](header: Header[B])(using merge: Merge[A, B]): Headers[merge.Out] = toHeaders :* header

  final def *:[B](header: Header[B])(using merge: Merge[B, A]): Headers[merge.Out] = header *: toHeaders

  final def toHeaders: Headers[A] = Headers(this)

  def decode(header: Option[String]): Codec.Result[A]

  def encode(a: A): Option[String]

object Header:
  final case class Default[A](name: CIString, codec: Codec[Data.Primitive, A], metadata: Metadata) extends Header[A]:
    override def modifyMetadata(f: Metadata => Metadata): Header[A] = copy(metadata = f(metadata))
    override def imap[B](f: A => B)(g: B => A): Header[B] = copy(codec = codec.imap(f)(g))
    override def optional: Header[Option[A]] = ??? // copy(codec = codec.nullable)
    override def decode(header: Option[String]): Codec.Result[A] = ??? // codec.parseNullable(header)
    override def encode(a: A): Option[String] = ??? // codec.printNullable(a)

  final case class Array[A](
      name: CIString,
      codec: Codec[Data.Array[Data.Primitive], A],
      metadata: Metadata
  ) extends Header[A]:
    override def modifyMetadata(f: Metadata => Metadata): Header[A] = copy(metadata = f(metadata))
    override def imap[B](f: A => B)(g: B => A): Header[B] = copy(codec = codec.imap(f)(g))
    override def optional: Header[Option[A]] = ??? // copy(codec = codec.nullable)
    override def decode(header: Option[String]): Codec.Result[A] = ???
    // codec.parseNullableArray(header.map(_.split(',').toVector))
    override def encode(a: A): Option[String] = ??? // codec.printNullableArray(a).map(_.mkString(","))

  final case class Object[A](
      name: CIString,
      codec: Codec[Data.Object[Data.Primitive], A],
      metadata: Metadata
  ) extends Header[A]:
    override def modifyMetadata(f: Metadata => Metadata): Header[A] = copy(metadata = f(metadata))
    override def imap[B](f: A => B)(g: B => A): Header[B] = copy(codec = codec.imap(f)(g))
    override def optional: Header[Option[A]] = ??? // copy(codec = codec.nullable)
    override def decode(header: Option[String]): Codec.Result[A] = ???
    // codec
    //   .parseNullableObject(
    //     header.map(_.split(',').map(_.split("=", 2)).collect { case SArray(key, value) => (key, value) }.toVector)
    //   )
    override def encode(a: A): Option[String] = ???
    // codec
    //   .printNullableObject(a)
    //   .map(_.map { case (key, value) => s"$key=$value" })
    //   .map(_.mkString(","))
