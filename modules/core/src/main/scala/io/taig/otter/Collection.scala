package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Validation
import io.taig.otter.validation.Violation
import cats.Id as Identity
import io.taig.otter.Codec.Result

sealed abstract class Collection[+F[+a] <: Data.Optional[a], +O <: Data, A] extends Codec[F, Data.Array[O], A]:
  self =>

  final override type Group = Data.Array[?]

  def codec: Codec[?, ?, ?]

  final override def modifyMetadata(f: Metadata => Metadata): Collection[F, O, A] = new Collection[F, O, A]:
    export self.{codec, decode, default, encode}
    override def metadata: Metadata = f(self.metadata)

  final override def modifyDefault(f: Option[A] => Option[A]): Collection[F, O, A] = new Collection[F, O, A]:
    export self.{codec, encode, metadata}
    override def default: Option[A] = f(self.default)
    override def decode(data: Option[Vector[Data]]): Codec.Result[A] = (data, default) match
      case (None, Some(default)) => default.valid
      case _                     => self.decode(data)

  final override def imap[B](f: A => B)(g: B => A): Collection[F, O, B] = ivalidate(Validation.lift(f))(g)

  final def to[B](using evidence: Evidence.Product.Aux[B, A]): Collection[F, O, B] = imap(evidence.from)(evidence.to)

  final override def ivalidate[B](validation: CodecValidation.Array[A, B])(f: B => A): Collection[F, O, B] =
    new Collection[F, O, B]:
      export self.{codec, metadata}
      override def default: Option[B] = self.default.flatMap(validation(_).toOption)
      override def decode(data: Option[Vector[Data]]): Codec.Result[B] =
        self.decode(data).andThen(validation(_).leftMap(Violations.root))
      override def encode(b: B): F[Data.Array[O]] = self.encode(f(b))

  override def optional: Collection[Data.Optional, O, Option[A]] = new Collection[Data.Optional, O, Option[A]]:
    export self.{codec, metadata}
    override def default: Option[Option[A]] = self.default.map(_.some)
    override def decode(data: Option[Vector[Data]]): Codec.Result[Option[A]] =
      data.fold(default.flatten.valid)(_ => self.decode(data).map(_.some))
    override def encode(a: Option[A]): Data.Optional[Data.Array[O]] = a.map(self.encode).getOrElse(Data.Null)

  override def decode(data: Data): Codec.Result[A] = data match
    case Data.Array(values) => decode(values.some)
    case Data.Null          => decode(none)
    case _ => Violations.rootNec(Violation(Constraint.Type("array"), actual = Data.String(data.name))).invalid

  def decode(data: Option[Vector[Data]]): Codec.Result[A]

object Collection:
  def apply[F[+a] <: Data.Optional[a], O <: Data, A](of: Codec[F, O, A]): Collection[Data.Required, F[O], Vector[A]] =
    new Collection[Data.Required, F[O], Vector[A]]:
      override def codec: Codec[?, ?, ?] = of
      override def metadata: Metadata = Metadata.Empty
      override def default: Option[Vector[A]] = None
      override def decode(data: Option[Vector[Data]]): Codec.Result[Vector[A]] = data
        .toValid(Violations.rootNec(Violation(Constraint.Type("array"), actual = Data.String("null"))))
        .andThen(_.zipWithIndex.traverse { case (data, index) => of.decode(data).leftMap(index /: _) })
      override def encode(as: Vector[A]): Data.Array[F[O]] = Data.Array(as.map(of.encode))

  // given invariant[F[+a <: Data] <: Data.Optional[a], O <: Data]
  //     : ValidationInvariant[[_] =>> Constraint.Collection, Collection[F, O, *]] with
  //   extension [A](self: Collection[F, O, A])
  //     override def ivalidate[B](validation: CodecValidation.Collection[A, B])(f: B => A): Collection[F, O, B] =
  //       self.ivalidate(validation)(f)
