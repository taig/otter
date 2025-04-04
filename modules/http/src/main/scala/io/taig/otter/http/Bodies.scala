// package io.taig.otter.http

// import cats.data.NonEmptyVector
// import cats.syntax.all.*
// import io.taig.otter.Codec
// import io.taig.otter.Convert
// import io.taig.otter.http.header.MediaRange
// import io.taig.otter.http.header.MediaType
// import org.typelevel.ci.*

// import java.nio.charset.Charset

// // TODO allow different codecs via taging, e.g. Bodies[Json[A] | Xml[B] | Csv[C]] (?)
// sealed abstract class Bodies[A]:
//   self =>

//   def toNev: NonEmptyVector[Body[?]]

//   final def imap[B](f: A => B)(g: B => A): Bodies[B] = new Bodies[B]:
//     export self.toNev
//     override def decode(contentType: MediaType, body: Array[Byte]): Codec.Result[Option[(MediaType, B)]] =
//       self.decode(contentType, body).map(_.map(_.map(f)))
//     override def decodeFirst(body: Array[Byte]): Codec.Result[(MediaType, B)] =
//       self.decodeFirst(body).map(_.map(f))
//     override def encode(accept: MediaRange, reject: List[MediaRange], b: B): Option[(MediaType, Array[Byte])] =
//       self.encode(accept, reject, g(b))
//     override def encode(contentType: MediaType, b: B): Option[Array[Byte]] = self.encode(contentType, g(b))
//     override def encodeFirst(charset: Option[Charset], b: B): (MediaType, Array[Byte]) = self.encodeFirst(charset, g(b))

//   final def to[B](convert: Convert[A, B]): Bodies[B] = imap(convert.to)(convert.from)

//   final def orElse[B](bodies: Bodies[B]): Bodies[Either[A, B]] = new Bodies[Either[A, B]]:
//     override def toNev: NonEmptyVector[Body[?]] = self.toNev.concatNev(bodies.toNev)
//     override def decode(contentType: MediaType, body: Array[Byte]): Codec.Result[Option[(MediaType, Either[A, B])]] =
//       self
//         .decode(contentType, body)
//         .andThen:
//           case Some((mediaType, a)) => (mediaType, a.asLeft).some.valid
//           case None                 => bodies.decode(contentType, body).map(_.map(_.map(_.asRight)))
//     override def decodeFirst(body: Array[Byte]): Codec.Result[(MediaType, Either[A, B])] =
//       self.decodeFirst(body).map(_.map(_.asLeft))
//     override def encode(
//         accept: MediaRange,
//         reject: List[MediaRange],
//         ab: Either[A, B]
//     ): Option[(MediaType, Array[Byte])] = ab.fold(self.encode(accept, reject, _), bodies.encode(accept, reject, _))
//     override def encode(contentType: MediaType, ab: Either[A, B]): Option[Array[Byte]] =
//       ab.fold(self.encode(contentType, _), bodies.encode(contentType, _))
//     override def encodeFirst(charset: Option[Charset], ab: Either[A, B]): (MediaType, Array[Byte]) =
//       ab.fold(self.encodeFirst(charset, _), bodies.encodeFirst(charset, _))

//   final def :+[B](body: Body[B]): Bodies[Either[A, B]] = orElse(body.toBodies)

//   final def +:[B](body: Body[B]): Bodies[Either[B, A]] = body.toBodies.orElse(this)

//   final def or(bodies: Bodies[A]): Bodies[A] = new Bodies[A]:
//     override def toNev: NonEmptyVector[Body[?]] = self.toNev.concatNev(bodies.toNev)
//     override def decode(contentType: MediaType, body: Array[Byte]): Codec.Result[Option[(MediaType, A)]] = self
//       .decode(contentType, body)
//       .andThen:
//         case a @ Some(_) => a.valid
//         case None        => bodies.decode(contentType, body)
//     override def decodeFirst(body: Array[Byte]): Codec.Result[(MediaType, A)] = self.decodeFirst(body)
//     override def encode(accept: MediaRange, reject: List[MediaRange], a: A): Option[(MediaType, Array[Byte])] =
//       self.encode(accept, reject, a).orElse(bodies.encode(accept, reject, a))
//     override def encode(contentType: MediaType, a: A): Option[Array[Byte]] =
//       self.encode(contentType, a).orElse(bodies.encode(contentType, a))
//     override def encodeFirst(charset: Option[Charset], a: A): (MediaType, Array[Byte]) = bodies.encodeFirst(charset, a)

//   final def +(body: Body[A]): Bodies[A] = or(body.toBodies)

//   def decode(contentType: MediaType, body: Array[Byte]): Codec.Result[Option[(MediaType, A)]]

//   def decodeFirst(body: Array[Byte]): Codec.Result[(MediaType, A)]

//   /** Use the first `Body` that matches the given `MediaRange` rules to encode the given `A`
//     *
//     * This method is intented to encode response bodies.
//     *
//     * @returns
//     *   `None` if no `Body` can fullfil the `Accept` rules, otherwise `Some` with the encoded result of the first `Body`
//     *   that matches the `Accept` rules
//     */
//   def encode(accept: MediaRange, reject: List[MediaRange], a: A): Option[(MediaType, Array[Byte])]

//   /** Use the first `Body` that matches the given `MediaType` to encode the given `A`
//     *
//     * This method is intented to encode request bodies.
//     */
//   def encode(contentType: MediaType, a: A): Option[Array[Byte]]

//   /** Use the first `Body` to encode the given `a`
//     *
//     * This method is intented to be used:
//     *   - For requests: when the client does not submit a `Content-Type` header
//     *   - For responses: when the client did neither submit an `Accept`, nor a `Content-Type` header
//     */
//   def encodeFirst(charset: Option[Charset], a: A): (MediaType, Array[Byte])

// object Bodies:
//   def apply[A](body: Body[A]): Bodies[A] = new Bodies[A]:
//     override def toNev: NonEmptyVector[Body[?]] = NonEmptyVector.one(body)
//     override def decode(contentType: MediaType, payload: Array[Byte]): Codec.Result[Option[(MediaType, A)]] =
//       if body.mediaType.tpe === contentType.tpe
//       then
//         body match
//           case body: Body.Strict[?] =>
//             val charset = contentType.parameters.get(ci"charset").reverse.collectFirstSome(loadCharset)
//             body.decode(charset, payload).tupleLeft(body.mediaType).map(_.some)
//           case _: Body.Streaming[?] => ???
//       else none.valid
//     override def decodeFirst(payload: Array[Byte]): Codec.Result[(MediaType, A)] = body match
//       case body: Body.Strict[?] => body.decode(charset = none, payload).tupleLeft(body.mediaType)
//       case _: Body.Streaming[?] => ???
//     override def encode(accept: MediaRange, reject: List[MediaRange], a: A): Option[(MediaType, Array[Byte])] =
//       Option.when(body.mediaType.satisfies(accept) && reject.forall(reject => !body.mediaType.satisfies(reject))):
//         val charset = accept.parameters.get(ci"charset").reverse.collectFirstSome(loadCharset)
//         body match
//           case body: Body.Strict[?] => (body.mediaType, body.encode(charset, a))
//           case _: Body.Streaming[?] => ???
//     override def encode(contentType: MediaType, a: A): Option[Array[Byte]] =
//       Option.when(contentType === body.mediaType):
//         val charset = contentType.parameters.get(ci"charset").reverse.collectFirstSome(loadCharset)
//         body match
//           case body: Body.Strict[?] => body.encode(charset, a)
//           case _: Body.Streaming[?] => ???
//     override def encodeFirst(charset: Option[Charset], a: A): (MediaType, Array[Byte]) = body match
//       case body: Body.Strict[?] => (body.mediaType, body.encode(charset, a))
//       case _: Body.Streaming[?] => ???
