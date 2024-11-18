package io.taig.otter.http
import cats.Show
import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Codec
import io.taig.otter.Constraint
import io.taig.otter.Data
import io.taig.otter.Metadata
import io.taig.otter.Violation
import io.taig.otter.Violations
import io.taig.otter.XPath

import java.util.regex.Pattern

sealed abstract class Segment[A] extends Product, Serializable:
  def name: String

  def matches(segment: String): Boolean

  final def toPath: Path[A] = Path(this)

  def encode(a: A): String

  def decode(value: String): Codec.Result[A]

  override def toString: String

object Segment:
  final case class Static(name: String) extends Segment[Unit]:
    override def matches(segment: String): Boolean = segment === name
    override def decode(value: String): Codec.Result[Unit] = Validated.cond(
      name === value,
      (),
      Violations.namespaceNec(
        XPath.Root / name,
        Violation(Constraint.Primitive.Matches(Pattern.compile(Pattern.quote(name))), actual = Data.String(value))
      )
    )

    override def encode(a: Unit): String = name

    override def toString: String = name

  sealed abstract class Parameter[A] extends Segment[A]:
    override def matches(segment: String): Boolean = true

    def codec: Codec[Data.Primitive | Data.Array[Data.Primitive] | Data.Object[Data.Primitive], ?]

    def metadata: Metadata

    def imap[B](f: A => B)(g: B => A): Segment[B]

    final override def toString: String = s"{$name}"

  object Parameter:
    final case class Default[A](name: String, codec: Codec[Data.Primitive, A], metadata: Metadata)
        extends Segment.Parameter[A]:
      override def imap[B](f: A => B)(g: B => A): Segment.Parameter[B] = copy(codec = codec.imap(f)(g))
      override def encode(a: A): String = ??? // codec.printRequired(a)
      override def decode(value: String): Codec.Result[A] = ??? // codec.parseRequired(value).leftMap(name /: _)

    final case class Array[A](name: String, codec: Codec[Data.Array[Data.Primitive], A], metadata: Metadata)
        extends Segment.Parameter[A]:
      override def imap[B](f: A => B)(g: B => A): Segment.Parameter[B] = copy(codec = codec.imap(f)(g))
      override def encode(a: A): String = ??? // codec.printArray(a).mkString(",")
      override def decode(value: String): Codec.Result[A] = ???
      // codec.parseArray(value.split(',').toVector).leftMap(name /: _)

    final case class Object[A](
        name: String,
        codec: Codec[Data.Object[Data.Primitive], A],
        metadata: Metadata
    ) extends Segment.Parameter[A]:
      override def imap[B](f: A => B)(g: B => A): Segment.Parameter[B] = copy(codec = codec.imap(f)(g))
      override def encode(a: A): String = ???
      // codec
      //   .printNullableObject(a)
      //   .fold("")(_.map { case (key, value) => (key, value) }.mkString(","))
      override def decode(value: String): Codec.Result[A] = ???
      // if value.isEmpty
      // then codec.parseNullableObject(none).leftMap(name /: _)
      // else
      //   codec
      //     .parseNullableObject(
      //       value.split(',').map(_.split("=", 2)).collect { case SArray(key, value) => (key, value) }.toVector.some
      //     )
      //     .leftMap(name /: _)

  given Show[Segment[?]] = Show.fromToString
