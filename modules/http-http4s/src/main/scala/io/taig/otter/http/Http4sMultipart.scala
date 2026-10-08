package io.taig.otter.http

import cats.data.Validated
import cats.effect.Concurrent
import cats.syntax.all.*
import fs2.Pure
import io.taig.data.Data
import io.taig.data.syntax.*
import io.taig.otter.Constraint
import io.taig.otter.Field
import io.taig.otter.Metadata
import io.taig.otter.Record
import io.taig.otter.Violations
import io.taig.otter.codec.Fields
import io.taig.otter.http.codec.Http4sBodyDecoder
import io.taig.otter.http.codec.Http4sBodyEncoder
import io.taig.otter.http.codec.Http4sPayload
import io.taig.validation.Violation
import org.http4s.Entity
import org.http4s.EntityEncoder
import org.http4s.Header
import org.http4s.Headers as Http4sHeaders
import org.http4s.multipart.Boundary
import org.http4s.multipart.Multipart as Http4sParts
import org.http4s.multipart.MultipartParser
import org.http4s.multipart.Part as Http4sPart
import org.typelevel.ci.CIString
import scodec.bits.ByteVector

import scala.annotation.tailrec
import scala.compiletime.asMatchable

/** Buffered multipart requests and responses, with the part interpreter in their requirement.
  *
  * Compose payload alphabets before registering them here. Nested multipart support is another such registration inside
  * `parts`; an alphabet registered beside this interpreter cannot answer for its parts. The entire entity is buffered,
  * using http4s's MIME parser and encoder, and streamed parts remain outside the supported requirement. Filenames
  * describe outgoing parts and never constrain incoming ones.
  */
object Http4sMultipart:
  type Parts[P[-_, +_]] = Multipart.Over[Http4sPayload.Supported[P]]

  def payload[P[-_, +_]](parts: Http4sPayload.Of[P]): Http4sPayload.Of[Http4sMultipart.Parts[P]] =
    Http4sPayload.entity(new Http4sPayload.Alphabet[Http4sMultipart.Parts[P]]:
      override private[http] def exclusive: Set[String] = Set("multipart")

      override def select[W, R, Q[-_, +_], A](payload: Http4sMultipart.Parts[P][W, R] | Q[W, R])(
          mine: Http4sMultipart.Parts[P][W, R] => A,
          theirs: Q[W, R] => A
      ): A = (payload.asMatchable: @unchecked) match
        case multipart: Http4sMultipart.Parts[P][W, R] @unchecked => mine(multipart)
        case other: Q[W, R] @unchecked => theirs(other))(new Http4sPayload.EntityCodec[Http4sMultipart.Parts[P]]:
      override def decode[F[_]: Concurrent, R](
          payload: Http4sMultipart.Parts[P][Nothing, R],
          mediaType: Option[MediaType],
          bytes: ByteVector
      ): F[Validated[DecodingFailure, R]] =
        Http4sMultipart
          .parse[F](mediaType, bytes)
          .flatMap:
            case Validated.Invalid(failure) => Validated.invalid[DecodingFailure, R](failure).pure[F]
            case Validated.Valid(values)    =>
              new Http4sMultipart.Reader[F, P](parts).record(payload.self.self, Fields.from(values)).map(_._2)

      override def encode[F[_]: Concurrent, W](
          payload: Http4sMultipart.Parts[P][W, Any],
          mediaType: MediaType,
          value: W
      ): F[Either[String, (MediaType, ByteVector)]] =
        new Http4sMultipart.Writer[F, P](parts)
          .record(payload.self.self, value)
          .flatMap:
            case Left(reason) => Left(reason).pure[F]
            case Right(parts) =>
              val boundary = Http4sMultipart.boundary(parts)
              val encoded =
                if parts.isEmpty then Http4sMultipart.utf8(s"--${boundary.value}--\r\n").pure[F]
                else
                  Http4sEnvelope.toBytes[F](
                    EntityEncoder.multipartEncoder[Pure].toEntity(Http4sParts(parts, boundary))
                  )
              encoded.map(bytes => Right((mediaType.withParameter("boundary", boundary.value), bytes))))

  private type Value = (Option[MediaType], ByteVector)
  private type Bodies[P[-_, +_]] = Body.Schema[Http4sPayload.Supported[P], *, *]

  private def utf8(value: String): ByteVector = ByteVector.view(value.getBytes(java.nio.charset.StandardCharsets.UTF_8))

  // http4s M48's quoted-string parser retains the backslashes of quoted pairs.
  private def unquote(value: String): String = value.replaceAll("\\\\(.)", "$1")

  private def quote(value: String): String = "\"" + value.replace("\\", "\\\\").replace("\"", "\\\"") + "\""

  private def syntax(bytes: ByteVector, cause: Option[Throwable] = None): DecodingFailure =
    DecodingFailure(
      Failure.Category.Syntax,
      Violations(Violation(Constraint.Generic.Type("multipart"), bytes.size.asData, None)),
      cause
    )

  private def parse[F[_]: Concurrent](
      mediaType: Option[MediaType],
      bytes: ByteVector
  ): F[Validated[DecodingFailure, Vector[(String, Http4sMultipart.Value)]]] =
    mediaType
      .flatMap(_.parameter("boundary"))
      .filter(value =>
        value.nonEmpty && value.length <= 70 && value.last != ' ' &&
          value.forall(char => char.isLetterOrDigit && char <= 127 || "'()+_,-./:=? ".contains(char))
      ) match
      case None           => Http4sMultipart.syntax(bytes).invalid.pure[F]
      case Some(boundary) =>
        // http4s M48 rejects a zero-part entity, although the schema can omit every part.
        val closing = Http4sMultipart.utf8(s"--$boundary--")
        if bytes == closing || bytes.startsWith(closing ++ Http4sMultipart.utf8("\r\n")) then
          Vector.empty[(String, Http4sMultipart.Value)].valid.pure[F]
        else
          fs2.Stream
            .chunk(fs2.Chunk.byteVector(bytes))
            .covary[F]
            .through(MultipartParser.parseToPartsStream[F](Boundary(boundary)))
            .evalMap: part =>
              Http4sEnvelope
                .toBytes(part.entity)
                .map(bytes =>
                  part.name.map(name =>
                    (Http4sMultipart.unquote(name), (Http4sEnvelope.toMediaType(part.headers), bytes))
                  )
                )
            .compile
            .toVector
            .attempt
            .map:
              case Right(parts) => parts.flatten.valid
              case Left(cause)  => Http4sMultipart.syntax(bytes, Some(cause)).invalid

  private def boundary(parts: Vector[Http4sPart[Pure]]): Boundary =
    val contents = parts.map(part =>
      val bytes = part.entity match
        case Entity.Strict(bytes)     => bytes
        case Entity.Empty             => ByteVector.empty
        case Entity.Streamed(body, _) => ByteVector.view(body.compile.toVector.toArray)
      Http4sMultipart.utf8(part.headers.headers.map(_.toString).mkString("\r\n")) ++ bytes
    )
    @tailrec def choose(index: Int): Boundary =
      val candidate = s"otter-boundary-$index"
      if contents.exists(_.indexOfSlice(Http4sMultipart.utf8(candidate)) >= 0) then choose(index + 1)
      else Boundary(candidate)
    choose(0)

  final private class Reader[F[_]: Concurrent, P[-_, +_]](payload: Http4sPayload.Of[P]):
    private val body = new Http4sBodyDecoder[F, P, Nothing](payload)

    def record[R](
        schema: Record[Part.Over[Http4sPayload.Supported[P]], Nothing, R],
        values: Fields[Http4sMultipart.Value]
    ): F[(Fields[Http4sMultipart.Value], Validated[DecodingFailure, R])] = schema match
      case Record.Empty                => (values, ().valid[DecodingFailure]).pure[F]
      case Record.Modify(self, f, _)   => record(self, values).map((rest, result) => (rest, result.map(f)))
      case Record.Product(left, right) =>
        record(left, values).flatMap: (rest, a) =>
          record(right, rest).map((rest, b) => (rest, (a, b).tupled))
      case Record.Root(reference) =>
        val self = reference.value.self.self
        val (rest, value) = values.take(self.name)
        field(self, value).map((rest, _))

    private def field[R](
        schema: Field[Http4sMultipart.Bodies[P], Nothing, R],
        value: Option[Http4sMultipart.Value]
    ): F[Validated[DecodingFailure, R]] = schema match
      case Field.Modify(self, f, _)             => field(self, value).map(_.map(f))
      case Field.Default(self, default, absent) =>
        if absent.matches(value, _._2.isEmpty) then default.value.valid[DecodingFailure].pure[F]
        else field(self, value)
      case Field.Optional(self, presence) =>
        if presence.absent.matches(value, _._2.isEmpty) then None.valid[DecodingFailure].pure[F]
        else field(self, value).map(_.map(Some(_)))
      case Field.Root(name, reference) =>
        val decoded = value match
          case Some(value) => body.decodeDetailed(reference.value, (value._1, Entity.strict(value._2)))
          case None        =>
            DecodingFailure(
              Failure.Category.Validation,
              Violations(Violation(Constraint.Generic.Required, Data.Null, None))
            ).invalid[R].pure[F]
        decoded.map(_.leftMap(failure => failure.copy(violations = name /: failure.violations)))

  final private class Writer[F[_]: Concurrent, P[-_, +_]](payload: Http4sPayload.Of[P]):
    private val body = new Http4sBodyEncoder[F, P, Nothing](payload)

    def record[W](
        schema: Record[Part.Over[Http4sPayload.Supported[P]], W, Any],
        value: W
    ): F[Either[String, Vector[Http4sPart[Pure]]]] = schema match
      case Record.Empty                => Right(Vector.empty).pure[F]
      case Record.Modify(self, _, g)   => record(self, g(value))
      case Record.Product(left, right) =>
        (record(left, value._1), record(right, value._2)).mapN((a, b) => (a, b).mapN(_ ++ _))
      case Record.Root(reference) =>
        val part = reference.value.self
        val filename = part.metadata.get(Http.Namespace, Metadata.Namespace.Global, HttpKeys.filename)
        field(part.self, filename, value)

    private def field[W](
        schema: Field[Http4sMultipart.Bodies[P], W, Any],
        filename: Option[String],
        value: W
    ): F[Either[String, Vector[Http4sPart[Pure]]]] = schema match
      case Field.Modify(self, _, g)       => field(self, filename, g(value))
      case Field.Default(self, _, _)      => field(self, filename, value)
      case Field.Optional(self, presence) =>
        value.fold(
          if presence.writesEmpty then
            member(self.name, filename, self.schema.value.mediaType, ByteVector.empty).map(Vector(_)).pure[F]
          else Right(Vector.empty).pure[F]
        )(field(self, filename, _))
      case Field.Root(name, reference) =>
        body
          .encode(reference.value, value)
          .flatMap:
            case Left(issue)                => Left(issue.show).pure[F]
            case Right((mediaType, entity)) =>
              Http4sEnvelope.toBytes(entity).map(bytes => member(name, filename, mediaType, bytes).map(Vector(_)))

    private def member(
        name: String,
        filename: Option[String],
        mediaType: MediaType,
        bytes: ByteVector
    ): Either[String, Http4sPart[Pure]] =
      if (name :: filename.toList).exists(_.exists(char => char < ' ' || char == 127)) then
        Left("Multipart names and filenames must not contain control characters")
      else
        // Content-Disposition's M48 renderer does not escape names and replaces characters in filenames.
        val disposition = s"form-data; name=${Http4sMultipart.quote(name)}" +
          filename.fold("")(value => s"; filename=${Http4sMultipart.quote(value)}")
        Right(
          Http4sPart(
            Http4sHeaders(
              Header.Raw(CIString("Content-Disposition"), disposition),
              Header.Raw(CIString("Content-Type"), mediaType.render)
            ),
            Entity.strict(bytes)
          )
        )
