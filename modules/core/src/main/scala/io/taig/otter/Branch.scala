package io.taig.otter

import io.taig.otter.Keys.*
import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import io.taig.otter.Codec.Result
import cats.Invariant
import cats.Id
import cats.Functor

sealed abstract class Branch[+O, A]:
  self =>

  def name: String

  def discriminator: Option[Discriminator]

  def codec: Codec[?, ?]

  def metadata: Metadata

  final def modifyMetadata(f: Metadata => Metadata): Branch[O, A] = new Branch[O, A]:
    export self.{codec, decode, discriminator, encode, name}
    override def metadata: Metadata = f(self.metadata)

  final def imap[B](f: A => B)(g: B => A): Branch[O, B] = new Branch[O, B]:
    export self.{codec, discriminator, metadata, name}
    override def decode(data: Data): Codec.Result[Option[B]] = self.decode(data).map(_.map(f))
    override def encode(b: B): O = self.encode(g(b))

  def decode(data: Data): Codec.Result[Option[A]]

  def encode(a: A): O

  // def decode(data: Chain[(String, Data)]): Codec.Result[Option[A]] =
  //   metadata(discriminator).getOrElse(Discriminator.Default) match
  //     case Discriminator.Nested(identifier, value) =>
  //       data
  //         .collectFirst { case (`identifier`, data) => data }
  //         .getOrElse(Data.Null)
  //         .match
  //           case Data.String(key) => (name === key).valid
  //           case data =>
  //             Violations.rootNec(Violation(Constraint.Type("string"), actual = Data.String(data.name))).invalid
  //         .andThen:
  //           case true =>
  //             decodeValue(data.collectFirst { case (`value`, data) => data }.getOrElse(Data.Null))
  //               .map(_.some)
  //               .leftMap(name /: _)
  //           case false => none.valid
  //     case Discriminator.Merged(identifier) =>
  //       val (key, remainders) = data.findWithRemainders { case (`identifier`, data) => data }

  //       key
  //         .getOrElse(Data.Null)
  //         .match
  //           case Data.String(key) => (name === key).valid
  //           case data =>
  //             Violations.rootNec(Violation(Constraint.Type("string"), actual = Data.String(data.name))).invalid
  //         .andThen:
  //           case true  => decodeValue(Data.Object(remainders)).map(_.some).leftMap(name /: _)
  //           case false => none.valid
  //     case Discriminator.Keyed =>
  //       decodeValue(data.collectFirst { case (key, data) if key === name => data }.getOrElse(Data.Null))
  //         .map(_.some)

  // protected def decodeValue(data: Data): Codec.Result[A]

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

  // final def encodeNested(identifier: String, value: String, a: A): Data.Object[Data.String | O] =
  //   Data.Object.of(identifier -> Data.String(name), value -> encodeUntagged(a))

  // final def encodeMerged[P <: Data](identifier: String, a: A)(using
  //     O <:< Data.Object[P]
  // ): Data.Object[Data.String | P] = encodeUntagged(a) ++ Data.Object.of(identifier -> Data.String(name))

  // final def encodeKeyed(a: A): Data.Object[O] = Data.Object.of(name -> encodeUntagged(a))

  // def encodeUntagged(a: A): O

object Branch:

// def nested[F[+_] <: Data, O, A](
//     identifier: String,
//     value: String,
//     name: String,
//     codec: Codec[F, O, A]
// ): Branch[Data.Object[Data.String | F[O]], A] = new Root[F, O, Data.Object[Data.String | F[O]], A](name, codec):
//   override def encode(a: A): Data.Object[Data.String | F[O]] =
//     Data.Object.of(identifier -> Data.String(this.name), value -> this.codec.encode(a))

  // def keyed[F[+_] <: Data, O, A](name: String, codec: Codec[F, O, A]): Branch[Data.Object[F[O]], A] = ???
  // new Root[F, O, Data.Object[F[O]], A](name, codec):
  //   override def encode(a: A): Data.Object[F[O]] = Data.Object.of(this.name -> this.codec.encode(a))

  private def apply[O <: Data, P, A](_name: String, _discriminator: Option[Discriminator], _codec: Codec[O, A])(
      f: Data => Codec.Result[Option[A]]
  )(g: A => P): Branch[P, A] = new Branch[P, A]:
    override def name: String = _name
    override def discriminator: Option[Discriminator] = _discriminator
    override def codec: Codec[?, ?] = _codec
    override def metadata: Metadata = Metadata.Empty
    override def decode(data: Data): Result[Option[A]] = f(data)
    override def encode(a: A): P = g(a)

  def nested[O <: Data, A](
      name: String,
      codec: Codec[O, A],
      discriminator: Discriminator.Nested
  ): Branch[Data.Object[Data.String | O], A] = Branch(name, discriminator.some, codec)(???): a =>
    Data.Object.of(discriminator.identifier -> Data.String(name), discriminator.value -> codec.encode(a))

  def merged[O <: Data.Optional[Data.Object[P]], P <: Data, A](
      name: String,
      codec: Codec[O, A],
      discriminator: Discriminator.Merged
  ): Branch[Data.Object[Data.String | P], A] = Branch(name, discriminator.some, codec)(???): a =>
    val data: Data.Null.type | Data.Object[P] = codec.encode(a)
    val result = Data.Object.one(discriminator.identifier, Data.String(name))

    data match
      case data: Data.Object[P] => data ++ result
      case Data.Null            => result

  def keyed[O <: Data, A](name: String, codec: Codec[O, A]): Branch[Data.Object[O], A] =
    Branch(name, Discriminator.Keyed.some, codec)(???): a =>
      Data.Object.of(name -> codec.encode(a))

  def untagged[O <: Data, A](name: String, codec: Codec[O, A]): Branch[O, A] =
    Branch(name, none, codec)(codec.decode(_).toOption.valid)(codec.encode)

  given [O <: Data]: Invariant[Branch[O, *]] with
    override def imap[A, B](fa: Branch[O, A])(f: A => B)(g: B => A): Branch[O, B] =
      fa.imap(f)(g)
