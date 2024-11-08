package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.Codec
import io.taig.otter.Convert
import io.taig.otter.Data
import io.taig.otter.Merge
import io.taig.otter.Metadata

sealed abstract class Query[A]:
  def name: String

  def codec: Codec[
    Data.Nullable,
    Data.Primitive | Data.Array[Data.Primitive] | Data.Object[Data.Nullable[Data.Primitive]],
    ?
  ]

  final def isOptional: Boolean = codec.isNullable

  def metadata: Metadata

  final def toQueries: Queries[A] = Queries(this)

  def imap[B](f: A => B)(g: B => A): Query[B]

  final def to[B](using convert: Convert[A, B]): Query[B] = imap(convert.to)(convert.from)

  def optional: Query[Option[A]]

  final def :*[B](query: Query[B])(using merge: Merge[A, B]): Queries[merge.Out] = toQueries :* query

  final def *:[B](query: Query[B])(using merge: Merge[B, A]): Queries[merge.Out] = query *: toQueries

  def decode(value: Query.Value): Codec.Result[A]

  def encode(a: A): Query.Value

object Query:
  enum Value:
    case Some(value: String)
    case None extends Value
    case Abscent extends Value

  final case class Default[A](name: String, codec: Codec[Data.Nullable, Data.Primitive, A], metadata: Metadata)
      extends Query[A]:
    override def imap[B](f: A => B)(g: B => A): Query[B] = copy(codec = codec.imap(f)(g))
    override def optional: Query[Option[A]] = copy(codec = codec.nullable)
    override def decode(value: Query.Value): Codec.Result[A] = value match
      case Query.Value.Some(value)                => codec.parseNullable(value.some)
      case Query.Value.None | Query.Value.Abscent => codec.parseNullable(none)
    override def encode(a: A): Query.Value = codec.printNullable(a).fold(Query.Value.None)(Query.Value.Some.apply)

  final case class Array[A](
      name: String,
      codec: Codec[Data.Nullable, Data.Array[Data.Primitive], A],
      metadata: Metadata
  ) extends Query[A]:
    override def imap[B](f: A => B)(g: B => A): Query[B] = copy(codec = codec.imap(f)(g))
    override def optional: Query[Option[A]] = copy(codec = codec.nullable)
    override def decode(value: Query.Value): Codec.Result[A] = value match
      case Query.Value.Some(value) => codec.parseNullableArray(value.split(',').toVector.some)
      case Query.Value.None        => codec.parseNullableArray(Vector.empty.some)
      case Query.Value.Abscent     => codec.parseNullableArray(none)
    override def encode(a: A): Query.Value = codec.printNullableArray(a) match
      case Some(Vector()) => Query.Value.None
      case Some(values)   => Query.Value.Some(values.mkString(","))
      case None           => Query.Value.Abscent

  final case class Object[A](
      name: String,
      codec: Codec[Data.Nullable, Data.Object[Data.Nullable[Data.Primitive]], A],
      metadata: Metadata
  ) extends Query[A]:
    override def imap[B](f: A => B)(g: B => A): Query[B] = copy(codec = codec.imap(f)(g))
    override def optional: Query[Option[A]] = copy(codec = codec.nullable)
    override def decode(value: Query.Value): Codec.Result[A] = ???
    override def encode(a: A): Query.Value = codec.printNullableObject(a) match
      case Some(Vector()) => Query.Value.None
      case Some(values)   => Query.Value.Some(values.map { case (key, value) => s"$key=$value" }.mkString(","))
      case None           => Query.Value.Abscent
