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

  def codec: Codec[?, ?]

  def metadata: Metadata

  final def modifyMetadata(f: Metadata => Metadata): Branch[O, A] = new Branch[O, A]:
    export self.{codec, encode, name}
    override def metadata: Metadata = f(self.metadata)

  final def imap[B](f: A => B)(g: B => A): Branch[O, B] = new Branch[O, B]:
    export self.{codec, metadata, name}
    override def encode(b: B): O = self.encode(g(b))
    // override def decodeValue(data: Data): Codec.Result[B] = self.decodeValue(data).map(f)
    // override def encodeUntagged(b: B): O = self.encodeUntagged(g(b))

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

  def merged[O <: Data.Optional[Data.Object[P]], P <: Data, A](
      identifier: String,
      name: String,
      codec: Codec[O, A]
  ): Branch[Data.Object[Data.String | P], A] = new Branch[Data.Object[Data.String | P], A]:
    override def name: String = ???
    override def metadata: Metadata = Metadata.Empty

    override def codec: Codec[O, A] = ???

    override def encode(a: A): Data.Object[Data.String | P] =
      val data: Data.Null.type | Data.Object[P] = codec.encode(a)

      data match
        case data: Data.Object[P] => data ++ Data.Object.one(identifier, Data.String(name))
        case Data.Null => Data.Object.one(identifier, Data.String(name))

// def merged[F[+a] <: Data.Optional[Data.Object[a]], O <: Data, A](identifier: String, name: String, codec: Codec[Id, F[O], A]): Branch[Data.Object[Data.String | F[O]], A] =
//   new Root[F, O, Data.Object[Data.String | F[P]], A](name, codec) {
//     override def encode(a: A): Data.Object[Data.String | F[P]] =
//       val x: F[O] = this.codec.encode(a)
//       x match
//         case _: Data.Optional[?] => ???
//         case _: Data.Value => ???

// y match
//   case Data.Null => Data.Object.one(identifier, Data.String(this.name))
//   case x: O => ???
// .++(Data.Object.one(identifier, Data.String(this.name)))
// }

// def untagged[F[+_], O, A](name: String, codec: Codec[F, O, A]): Branch[F[O], A] =
//   new Root[F, O, F[O], A](name, codec):
//     override def encode(a: A): F[O] = this.codec.encode(a)

given [O <: Data]: Invariant[Branch[O, *]] with
  override def imap[A, B](fa: Branch[O, A])(f: A => B)(g: B => A): Branch[O, B] =
    fa.imap(f)(g)
