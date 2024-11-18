package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.Codec
import io.taig.otter.Convert
import io.taig.otter.Data
import io.taig.otter.Merge
import io.taig.otter.Metadata
import io.taig.otter.collectFirstWithRemainders

sealed abstract class Query[A]:
  self =>

  def name: String

  def codec: Codec[?, ?]

  def metadata: Metadata

  final def isNullable: Boolean = codec.isNullable
  final def isRequired: Boolean = codec.isRequired

  def modifyMetadata(f: Metadata => Metadata): Query[A]

  def imap[B](f: A => B)(g: B => A): Query[B]

  def to[B](using convert: Convert[A, B]): Query[B]

  final def toQueries: Queries[A] = Queries(this)

  final def :*[B](query: Query[B])(using merge: Merge[A, B]): Queries[merge.Out] = toQueries :* query

  final def *:[B](query: Query[B])(using merge: Merge[B, A]): Queries[merge.Out] = query *: toQueries

  def decode(values: Vector[(String, Option[String])]): (Vector[(String, Option[String])], Codec.Result[A])

  def encode(a: A): Vector[(String, Option[String])]

object Query:
  sealed abstract class Required[A] extends Query[A]:
    override def modifyMetadata(f: Metadata => Metadata): Query.Required[A]
    override def imap[B](f: A => B)(g: B => A): Query.Required[B]
    override def to[B](using convert: Convert[A, B]): Query.Required[B]
    def nullable: Query[Option[A]]
    def optional: Query[Option[A]]

  object Required:
    final private[otter] case class Primitive[A](
        name: String,
        codec: Codec[Data.Primitive, A],
        metadata: Metadata
    ) extends Query.Required[A]:
      override def modifyMetadata(f: Metadata => Metadata): Query.Required[A] = copy(metadata = f(metadata))
      override def imap[B](f: A => B)(g: B => A): Query.Required[B] = copy(codec = codec.imap(f)(g))
      override def to[B](using convert: Convert[A, B]): Query.Required[B] = imap(convert.to)(convert.from)
      override def nullable: Query[Option[A]] = Query.Primitive(name, codec = codec.nullable, metadata, nullable = true)
      override def optional: Query[Option[A]] =
        Query.Primitive(name, codec = codec.nullable, metadata, nullable = false)
      override def decode(
          values: Vector[(String, Option[String])]
      ): (Vector[(String, Option[String])], Codec.Result[A]) =
        val (remainders, value) = values.collectFirstWithRemainders { case (`name`, value) => value }
        (remainders, value.flatten.fold(codec.decode(Data.Null))(codec.parse).leftMap(name /: _))
      override def encode(a: A): Vector[(String, Option[String])] = Vector((name, codec.print(a).some))

    final private[otter] case class Array[A](
        name: String,
        codec: Codec[Data.Array[Data.Primitive], A],
        metadata: Metadata,
        delimiter: Delimiter
    ) extends Query.Required[A]:
      override def modifyMetadata(f: Metadata => Metadata): Query.Required[A] = copy(metadata = f(metadata))
      override def imap[B](f: A => B)(g: B => A): Query.Required[B] = copy(codec = codec.imap(f)(g))
      override def to[B](using convert: Convert[A, B]): Query.Required[B] = imap(convert.to)(convert.from)
      override def nullable: Query[Option[A]] =
        Query.Array(name, codec = codec.nullable, metadata, delimiter, nullable = true)
      override def optional: Query[Option[A]] =
        Query.Array(name, codec = codec.nullable, metadata, delimiter, nullable = false)
      override def decode(
          values: Vector[(String, Option[String])]
      ): (Vector[(String, Option[String])], Codec.Result[A]) =
        val (remainders, value) = values.collectFirstWithRemainders { case (`name`, value) => value }
        (
          remainders,
          value.flatten
            .fold(codec.decode(Data.Null))(value => codec.parseArray(delimiter.decode(value)))
            .leftMap(name /: _)
        )
      override def encode(a: A): Vector[(String, Option[String])] = Vector(
        (name, delimiter.encode(codec.printArray(a)).some)
      )

    final private[otter] case class Object[A](
        name: String,
        codec: Codec[Data.Object[Data.Nullable[Data.Primitive]], A],
        metadata: Metadata
    ) extends Query.Required[A]:
      override def modifyMetadata(f: Metadata => Metadata): Query.Required[A] = copy(metadata = f(metadata))
      override def imap[B](f: A => B)(g: B => A): Query.Required[B] = copy(codec = codec.imap(f)(g))
      override def to[B](using convert: Convert[A, B]): Query.Required[B] = imap(convert.to)(convert.from)
      override def nullable: Query[Option[A]] = ???
      override def optional: Query[Option[A]] = ???
      override def decode(
          values: Vector[(String, Option[String])]
      ): (Vector[(String, Option[String])], Codec.Result[A]) =
        val (remainders, value) = values.collectFirstWithRemainders { case (`name`, value) => value }
        val result = value.flatten match
          case Some(value) =>
            val obj = Data.Object(FormData.parse(value).toVector.map {
              case (name, Some(value)) => (name, Data.String(value))
              case (name, None)        => (name, Data.Null)
            })
            codec.decode(obj)
          case None => codec.decode(Data.Null)
        (remainders, result.leftMap(name /: _))
      override def encode(a: A): Vector[(String, Option[String])] =
        Vector((name, Printers(FormData(codec.printObject(a))).some))

  final private[otter] case class Primitive[A](
      name: String,
      codec: Codec[Data.Nullable[Data.Primitive], A],
      metadata: Metadata,
      nullable: Boolean
  ) extends Query[A]:
    override def modifyMetadata(f: Metadata => Metadata): Query[A] = copy(metadata = f(metadata))
    override def imap[B](f: A => B)(g: B => A): Query[B] = copy(codec = codec.imap(f)(g))
    override def to[B](using convert: Convert[A, B]): Query[B] = imap(convert.to)(convert.from)
    override def decode(
        values: Vector[(String, Option[String])]
    ): (Vector[(String, Option[String])], Codec.Result[A]) =
      val (remainders, value) = values.collectFirstWithRemainders { case (`name`, value) => value }
      (remainders, codec.parseNullable(value.flatten).leftMap(name /: _))
    override def encode(a: A): Vector[(String, Option[String])] = codec.printNullable(a) match
      case Some(value)      => Vector((name, value.some))
      case None if nullable => Vector((name, none))
      case None             => Vector.empty

  final private[otter] case class Array[A](
      name: String,
      codec: Codec[Data.Nullable[Data.Array[Data.Primitive]], A],
      metadata: Metadata,
      delimiter: Delimiter,
      nullable: Boolean
  ) extends Query[A]:
    override def modifyMetadata(f: Metadata => Metadata): Query[A] = copy(metadata = f(metadata))
    override def imap[B](f: A => B)(g: B => A): Query[B] = copy(codec = codec.imap(f)(g))
    override def to[B](using convert: Convert[A, B]): Query[B] = imap(convert.to)(convert.from)
    override def decode(
        values: Vector[(String, Option[String])]
    ): (Vector[(String, Option[String])], Codec.Result[A]) = ???
    override def encode(a: A): Vector[(String, Option[String])] = codec.printNullableArray(a) match
      case Some(values)     => Vector((name, delimiter.encode(values).some))
      case None if nullable => Vector((name, none))
      case None             => Vector.empty

  final private[otter] case class Object[A](
      name: String,
      codec: Codec[Data.Nullable[Data.Object[Data.Nullable[Data.Primitive]]], A],
      metadata: Metadata,
      nullable: Boolean
  ) extends Query[A]:
    override def modifyMetadata(f: Metadata => Metadata): Query[A] = copy(metadata = f(metadata))
    override def imap[B](f: A => B)(g: B => A): Query[B] = copy(codec = codec.imap(f)(g))
    override def to[B](using convert: Convert[A, B]): Query[B] = imap(convert.to)(convert.from)
    override def decode(
        values: Vector[(String, Option[String])]
    ): (Vector[(String, Option[String])], Codec.Result[A]) =
      val (remainders, value) = values.collectFirstWithRemainders { case (`name`, value) => value }
      val result = value.flatten match
        case Some(value) =>
          val obj = Data.Object(FormData.parse(value).toVector.map {
            case (name, Some(value)) => (name, Data.String(value))
            case (name, None)        => (name, Data.Null)
          })
          codec.decode(obj)
        case None => codec.decode(Data.Null)
      (remainders, result.leftMap(name /: _))
    override def encode(a: A): Vector[(String, Option[String])] =
      codec.printNullableObject(a) match
        case Some(values)     => Vector((name, Printers(FormData(values)).some))
        case None if nullable => Vector((name, none))
        case None             => Vector.empty

  given [A]: Metadata.Ops[Query[A]] with
    extension (self: Query[A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Query[A] = self.modifyMetadata(f)
