package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.Codec
import io.taig.otter.Convert
import io.taig.otter.Data
import io.taig.otter.Merge
import io.taig.otter.collectFirstWithRemainders
import io.taig.otter.Metadata
import io.taig.otter.Null
import io.taig.otter.Keys

sealed abstract class Query[A]:
  self =>

  def name: String

  def codec: Codec[Query.Of, ?]

  final def isNullable: Boolean = codec.isNullable

  def metadata: Metadata

  final def modifyMetadata(f: Metadata => Metadata): Query[A] = new Query[A]:
    export self.{codec, decode, encode, imap, name, optional}
    override def metadata: Metadata = f(self.metadata)

  final def nulls: Null = metadata.get(Keys.nulls).getOrElse(Null.Default)
  final def nulls(value: Null): Query[A] = self(Keys.nulls, value)
  final def hideNulls: Query[A] = nulls(Null.Hide)

  def imap[B](f: A => B)(g: B => A): Query[B]

  final def to[B](using convert: Convert[A, B]): Query[B] = imap(convert.to)(convert.from)

  def optional: Query[Option[A]]

  final def toQueries: Queries[A] = Queries(this)

  final def :*[B](query: Query[B])(using merge: Merge[A, B]): Queries[merge.Out] = toQueries :* query

  final def *:[B](query: Query[B])(using merge: Merge[B, A]): Queries[merge.Out] = query *: toQueries

  def decode(values: Vector[(String, Option[String])]): (Vector[(String, Option[String])], Codec.Result[A])

  def encode(a: A): Vector[(String, Option[String])]

object Query:
  // TODO ????
  type Of = Data.Nullable[Data.Primitive | Data.Array[Data.Primitive] | Data.Object[Data.Primitive]]

  // final private case class Primitive[A](name: String, codec: Codec[Data.Nullable[Data.Primitive], A]) extends Query[A]:
  //   override def metadata: Metadata = Metadata.Empty
  //   override def imap[B](f: A => B)(g: B => A): Query[B] = copy(codec = codec.imap(f)(g))
  //   override def optional: Query[Option[A]] = copy(codec = codec.nullable)
  //   override def decode(values: Vector[(String, Option[String])]): (Vector[(String, Option[String])], Codec.Result[A]) =
  //     val (remainders, value) = values.collectFirstWithRemainders { case (`name`, value) => value }
  //     (remainders, codec.parseNullable(value.flatten).leftMap(name /: _))
  //   override def encode(a: A): Vector[(String, Option[String])] = codec.printNullable(a) match
  //     case Some(value)                 => Vector((name, value.some))
  //     case None if nulls === Null.Show => Vector((name, none))
  //     case None                        => Vector.empty

  // final private case class Array[A](
  //     name: String,
  //     codec: Codec[Data.Nullable[Data.Array[Data.Primitive]], A]
  // ) extends Query[A]:
  //   override def metadata: Metadata = Metadata.Empty
  //   override def imap[B](f: A => B)(g: B => A): Query[B] = copy(codec = codec.imap(f)(g))
  //   override def optional: Query[Option[A]] = copy(codec = codec.nullable)
  //   override def decode(values: Vector[(String, Option[String])]): (Vector[(String, Option[String])], Codec.Result[A]) =
  //     val (remainders, value) = values.collectFirstWithRemainders { case (`name`, value) => value }
  //     // (remainders, codec.parseNullableArray(value.map(_.split(",").toVector)).leftMap(name /: _))
  //     ???
  //   override def encode(a: A): Vector[(String, Option[String])] =
  //     ??? // Vector.from(codec.printNullableArray(a).map(_.mkString(","))).tupleLeft(name)

  // final private case class Object[A](
  //     name: String,
  //     codec: Codec[Data.Nullable[Data.Object[Data.Primitive]], A]
  // ) extends Query[A]:
  //   override def metadata: Metadata = Metadata.Empty
  //   override def imap[B](f: A => B)(g: B => A): Query[B] = copy(codec = codec.imap(f)(g))
  //   override def optional: Query[Option[A]] = copy(codec = codec.nullable)
  //   override def decode(values: Vector[(String, Option[String])]): (Vector[(String, Option[String])], Codec.Result[A]) =
  //     ???
  //   override def encode(a: A): Vector[(String, Option[String])] = ???
  //   // Vector
  //   // .from(codec.printNullableObject(a).map(_.map { case (key, value) => s"$key=$value" }.mkString(",")))
  //   // .tupleLeft(name)

  // def primitive[A](name: String, codec: Codec[Data.Nullable[Data.Primitive], A]): Query[A] = Primitive(name, codec)
  // def array[A](name: String, codec: Codec[Data.Nullable[Data.Array[Data.Primitive]], A]): Query[A] = Array(name, codec)
  // def obj[A](name: String, codec: Codec[Data.Nullable[Data.Object[Data.Primitive]], A]): Query[A] = Object(name, codec)

  given [A]: Metadata.Ops[Query[A]] with
    extension (self: Query[A])
      override def metadata: Metadata = self.metadata
      override def modifyMetadata(f: Metadata => Metadata): Query[A] = self.modifyMetadata(f)
