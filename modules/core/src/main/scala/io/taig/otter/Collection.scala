package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Validation
import io.taig.otter.validation.Violation
import cats.Id

sealed abstract class Collection[+F[+a <: Data] <: Data.Optional[a], +O <: Data, A] extends Codec[F, Data.Array[O], A]:
  self =>

  def codec: Codec[?, ?, ?]

  final override def modifyMetadata(f: Metadata => Metadata): Collection[F, O, A] = new Collection[F, O, A]:
    export self.{codec, decode, default, encode}
    override def metadata: Metadata = f(self.metadata)

  final override def modifyDefault(f: Option[A] => Option[A]): Collection[F, O, A] = new Collection[F, O, A]:
    export self.{codec, encode, metadata}
    override def default: Option[A] = f(self.default)
    override def decode(data: Data): Codec.Result[A] = (data, default) match
      case (Data.Null, Some(default)) => default.valid
      case _                          => self.decode(data)

  final override def imap[B](f: A => B)(g: B => A): Collection[F, O, B] = ivalidate(Validation.lift(f))(g)

  final def ivalidate[B](validation: CodecValidation.Collection[A, B])(f: B => A): Collection[F, O, B] =
    new Collection[F, O, B]:
      export self.{codec, metadata}
      override def default: Option[B] = self.default.flatMap(validation(_).toOption)
      override def decode(data: Data): Codec.Result[B] =
        self.decode(data).andThen(validation(_).leftMap(Violations.root))
      override def encode(b: B): F[Data.Array[O]] = self.encode(f(b))

  override def optional: Collection[Data.Optional, O, Option[A]] = new Collection[Data.Optional, O, Option[A]]:
    export self.{codec, metadata}
    override def default: Option[Option[A]] = self.default.map(_.some)
    override def decode(data: Data): Codec.Result[Option[A]] =
      data.toValue.fold(default.flatten.valid)(_ => self.decode(data).map(_.some))
    override def encode(a: Option[A]): Data.Optional[Data.Array[O]] = a.map(self.encode).getOrElse(Data.Null)

object Collection:
  def apply[F[+a <: Data] <: Data.Optional[a], O <: Data.Value, A](
      codec: Codec[F, O, A]
  ): Collection[Id, F[O], Vector[A]] =
    val _codec = codec

    new Collection[Id, F[O], Vector[A]]:
      override def codec: Codec[F, O, A] = _codec
      override def metadata: Metadata = Metadata.Empty
      override def default: Option[Vector[A]] = None
      override def decode(data: Data): Codec.Result[Vector[A]] = data.toArray
        .toValid(Violations.rootNec(Violation(Constraint.Type("array"), actual = Data.String(data.name))))
        .andThen(_.values.traverse(codec.decode))
      override def encode(as: Vector[A]): Data.Array[F[O]] = Data.Array(as.map(codec.encode))

  // given invariant[O <: Data.Optional[Data.Array[?]]]
  //     : ValidationInvariant[[_] =>> Constraint.Collection, Collection[O, *]] with
  //   extension [A](self: Collection[O, A])
  //     override def ivalidate[B](validation: CodecValidation.Collection[A, B])(f: B => A): Collection[O, B] =
  //       self.ivalidate(validation)(f)
