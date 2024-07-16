package io.taig.otter

import cats.data.Validated

import io.taig.otter.validation.Violations
import cats.syntax.all.*
import io.taig.otter.validation.Violation
import io.taig.otter.validation.Validation

sealed abstract class Collection[+O, A] extends Codec[O, A]:
  self =>

  def schema: Codec[?, ?]

  final override def imap[B](f: A => B)(g: B => A): Collection[O, B] = ivalidate(Validation.lift(f))(g)

  final def ivalidate[B](validation: SchemaValidation.Collection[A, B])(f: B => A): Collection[O, B] =
    new Collection[O, B]:
      export self.{metadata, schema}

      override def decodeArray(data: Option[Data.Array]): Codec.Result[Data, B] =
        self.decodeArray(data).andThen(validation(_).leftMap(Violations.root))

      override def encodeArray(b: B): Option[Data.Array] = self.encodeArray(f(b))

  final override def optional: Collection[O, Option[A]] = new Collection[O, Option[A]]:
    export self.{metadata, schema}

    override def decodeArray(data: Option[Data.Array]): Codec.Result[Data, Option[A]] =
      data.fold(none.valid)(_ => self.decodeArray(data).map(_.some))

    override def encodeArray(a: Option[A]): Option[Data.Array] = a.flatMap(self.encodeArray)

  final override def update(f: Metadata => Metadata): Collection[O, A] = new Collection[O, A]:
    export self.{decodeArray, encodeArray, schema}
    override def metadata: Metadata = f(self.metadata)

  final override def decodeOption(data: Option[Data.Value]): Codec.Result[Data, A] = data match
    case Some(data: Data.Array) => decodeArray(data.some)
    case Some(data) =>
      Violations.rootNec(Violation(Constraint.Type(name = "array"), actual = Data.String(data.name))).invalid
    case None => decodeArray(None)

  def decodeArray(data: Option[Data.Array]): Codec.Result[Data, A]

  final override def encodeOption(a: A): Option[Data.Value] = encodeArray(a)

  def encodeArray(a: A): Option[Data.Array]

object Collection:
  def apply[A](value: Codec[?, A]): Collection[value.type, Vector[A]] = new Collection[value.type, Vector[A]]:
    override def metadata: Metadata = Metadata.Empty
    override def schema: Codec[?, A] = value
    override def decodeArray(data: Option[Data.Array]): Codec.Result[Data, Vector[A]] = data
      .toValid(Violations.rootNec(Violation(Constraint.Type(name = "array"), actual = Data.String("null"))))
      .andThen(_.values.traverse(schema.decode(_)))
    override def encodeArray(a: Vector[A]): Option[Data.Array] = Data.Array(a.map(value.encode)).some
