package io.taig.otter

import io.taig.otter.Keys.*
import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import io.taig.otter.Codec.Result
import cats.Invariant

sealed abstract class Branch[+O <: Data[?], A]:
  self =>

  def name: String

  def codec: Codec[?, ?]

  def metadata: Metadata

  final def modifyMetadata(f: Metadata => Metadata): Branch[O, A] = new Branch[O, A]:
    export self.{codec, decodeValue, encodeValue, name}
    override def metadata: Metadata = f(self.metadata)

  final def imap[B](f: A => B)(g: B => A): Branch[O, B] = new Branch[O, B]:
    export self.{codec, metadata, name}
    override def decodeValue(data: Data[?]): Codec.Result[B] = self.decodeValue(data).map(f)
    override def encodeValue(b: B): O = self.encodeValue(g(b))

  def decode(data: Chain[(String, Data[?])]): Codec.Result[Option[A]] =
    metadata(discriminator).getOrElse(Discriminator.Default) match
      case Discriminator.Nested(identifier, value) =>
        data
          .collectFirst { case (`identifier`, data) => data }
          .getOrElse(Data.Null)
          .match
            case Data.String(key) => (name === key).valid
            case data =>
              Violations.rootNec(Violation(Constraint.Type("string"), actual = Data.String(data.name))).invalid
          .andThen:
            case true =>
              decodeValue(data.collectFirst { case (`value`, data) => data }.getOrElse(Data.Null))
                .map(_.some)
                .leftMap(name /: _)
            case false => none.valid
      case Discriminator.Merged(identifier) =>
        val (key, remainders) = data.findWithRemainders { case (`identifier`, data) => data }

        key
          .getOrElse(Data.Null)
          .match
            case Data.String(key) => (name === key).valid
            case data =>
              Violations.rootNec(Violation(Constraint.Type("string"), actual = Data.String(data.name))).invalid
          .andThen:
            case true  => decodeValue(Data.Object(remainders)).map(_.some).leftMap(name /: _)
            case false => none.valid
      case Discriminator.Keyed =>
        decodeValue(data.collectFirst { case (key, data) if key === name => data }.getOrElse(Data.Null))
          .map(_.some)

  protected def decodeValue(data: Data[?]): Codec.Result[A]

  // def encode(a: A): Data.Object[Data.String | O] = metadata(discriminator).getOrElse(Discriminator.Default) match
  //   case Discriminator.Nested(identifier, value) =>
  //     Data.Object.of(identifier -> Data.String(name), value -> encodeValue(a))
  //   case Discriminator.Merged(identifier) =>
  //     encodeValue(a) match
  //       case values: (O & Data.Object[?]) => Data.Object.of(identifier -> Data.String(name))
  //       case _ => Data.Object.of(identifier -> Data.String(name))
  //     // Data.Object.of(identifier -> Data.String(name))
  //     // encodeValue(a).toObject.getOrElse(Data.Object.Empty) ++
  //     //   Data.Object.of(identifier -> Data.String(name))
  //   case Discriminator.Keyed => Data.Object.of(name -> encodeValue(a))

  final def encodeNested(identifier: String, value: String, a: A): Data.Object[Data.String | O] =
    Data.Object.of(identifier -> Data.String(name), value -> encodeValue(a))

  final def encodeMerged[P <: Data[?]](identifier: String, a: A)(using
      O <:< Data.Object[P]
  ): Data.Object[Data.String | P] = encodeValue(a) ++ Data.Object.of(identifier -> Data.String(name))

  final def encodeKeyed(a: A): Data.Object[O] = Data.Object.of(name -> encodeValue(a))

  protected def encodeValue(a: A): O

object Branch:
  def apply[O <: Data[?], A](identifier: String, of: Codec[O, A]): Branch[O, A] = new Branch[O, A]:
    override def name: String = identifier
    override def codec: Codec[?, A] = of
    override def metadata: Metadata = Metadata.Empty
    override def decodeValue(data: Data[?]): Codec.Result[A] = codec.decode(data)
    override def encodeValue(a: A): O = of.encode(a)

  given [O <: Data[?]]: Invariant[Branch[O, *]] with
    override def imap[A, B](fa: Branch[O, A])(f: A => B)(g: B => A): Branch[O, B] =
      fa.imap(f)(g)
