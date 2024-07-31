package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.Metadata
import io.taig.otter.Codec
import io.taig.otter.Data
import scala.Array as SArray
import io.taig.otter.Evidence

sealed abstract class Query[A]:
  def name: String

  def codec: Codec[
    Data.Optional,
    Data.Primitive | Data.Array[Data.Primitive] | Data.Object[Data.Optional[Data.Primitive]],
    ?
  ]

  def metadata: Metadata

  final def toQueries: Queries[A] = Queries(this)

  def imap[B](f: A => B)(g: B => A): Query[B]

  final def :*[B](query: Query[B])(using merge: Evidence.Merge[A, B]): Queries[merge.Out] = toQueries :* query

  final def *:[B](query: Query[B])(using merge: Evidence.Merge[B, A]): Queries[merge.Out] = query *: toQueries

  def decode(value: Option[String]): Codec.Result[A]

  def encode(a: A): Option[String]

object Query:
  final case class Default[A](name: String, codec: Codec[Data.Optional, Data.Primitive, A], metadata: Metadata)
      extends Query[A]:
    override def imap[B](f: A => B)(g: B => A): Query[B] = copy(codec = codec.imap(f)(g))
    override def decode(value: Option[String]): Codec.Result[A] = codec.parseOptional(value)
    override def encode(a: A): Option[String] = codec.printOptional(a)

  final case class Array[A](
      name: String,
      codec: Codec[Data.Optional, Data.Array[Data.Primitive], A],
      metadata: Metadata
  ) extends Query[A]:
    override def imap[B](f: A => B)(g: B => A): Query[B] = copy(codec = codec.imap(f)(g))
    override def decode(value: Option[String]): Codec.Result[A] =
      codec.parseOptionalArray(value.map(_.split(',').toVector))
    override def encode(a: A): Option[String] = codec.printOptionalArray(a).map(_.mkString(","))

  final case class Object[A](
      name: String,
      codec: Codec[Data.Optional, Data.Object[Data.Optional[Data.Primitive]], A],
      metadata: Metadata
  ) extends Query[A]:
    override def imap[B](f: A => B)(g: B => A): Query[B] = copy(codec = codec.imap(f)(g))
    override def decode(value: Option[String]): Codec.Result[A] = codec
      .parseOptionalObject(
        value.map(_.split(',').map(_.split("=", 2)).collect { case SArray(key, value) => (key, value) }.toVector)
      )
    override def encode(a: A): Option[String] = codec
      .printOptionalObject(a)
      .map(_.map { case (key, value) => s"$key=$value" })
      .map(_.mkString(","))
