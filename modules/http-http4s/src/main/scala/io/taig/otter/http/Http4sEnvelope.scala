package io.taig.otter.http

import cats.data.Chain
import cats.effect.Concurrent
import cats.syntax.all.*
import org.http4s.Entity
import org.http4s.Header as Http4sHeader
import org.http4s.Headers as Http4sHeaders
import org.http4s.Method as Http4sMethod
import org.http4s.ParseResult
import org.http4s.Query as Http4sQuery
import org.http4s.Response as Http4sResponse
import org.http4s.Status
import org.http4s.Uri
import org.typelevel.ci.CIString
import scodec.bits.ByteVector

import java.util.Locale

/** http4s's envelope types, as the slices `http`'s codecs speak, and back.
  *
  * There is remarkably little here, and that is the finding rather than an omission. `http` describes a path as a
  * `Vector[String]`, a query string as a `Chain[(String, Option[String])]` and a header set as a
  * `Chain[(String, String)]` -- the narrowest thing each position could be -- on the bet that a backend would then need
  * only trivial functions to meet them. http4s's `Query` is a `Vector[(String, Option[String])]` in both directions and
  * its `Headers` a list of raw name and value pairs, so two of the three are a container conversion and nothing more.
  * The bet paid.
  *
  * Percent encoding is the one place a decision is left, and it belongs here rather than in `http`, which said so:
  * "percent encoding and the `/` between them are the backend's, which owns the URL type it is building".
  */
object Http4sEnvelope:
  def toMethod(method: Http4sMethod): Method = Method(method.name)

  /** `ParseResult` rather than a total function, because http4s parses a method against the token grammar and `http`
    * does not: [[Method]] is a `String` wrapper precisely so that a method it has never heard of can be named.
    * Everything that grammar rejects would be unsendable anyway.
    */
  def toHttp4sMethod(method: Method): ParseResult[Http4sMethod] = Http4sMethod.fromString(method.name)

  /** The segments, decoded.
    *
    * A `Vector[String]` of what stood between the slashes, which is what [[io.taig.otter.http.codec.PathDecoder]]
    * reads. Decoding here is what makes a segment holding an encoded `/` arrive as one segment rather than two.
    */
  def toPath(path: Uri.Path): Vector[String] = path.segments.map(_.decoded())

  /** The segments, encoded, as an absolute path.
    *
    * `Uri.Path.Segment.apply` percent encodes, so a value holding a `/` survives as one segment -- which is the whole
    * reason [[io.taig.otter.http.codec.PathEncoder]] hands over a `Vector[String]` and not a joined `String`. Building
    * the path from its segments rather than rendering and re-parsing is what keeps that true.
    */
  def toHttp4sPath(segments: Vector[String]): Uri.Path =
    Uri.Path(segments.map(Uri.Path.Segment.apply), absolute = true)

  /** The name and value pairs, with a name given without a value kept apart from one given an empty one.
    *
    * `Query.toVector` and not `multiParams`, which collapses `?a` into `?a=` and would make the flag `?verbose` a
    * different request from the one that was sent. [[io.taig.otter.http.codec.QueriesDecoder]] gives all three of
    * `?a=1`, `?a=` and `?a` a meaning, so all three have to survive the crossing.
    */
  def toQueries(query: Http4sQuery): Chain[(String, Option[String])] = Chain.fromSeq(query.toVector)

  def toHttp4sQuery(queries: Chain[(String, Option[String])]): Http4sQuery =
    Http4sQuery.fromVector(queries.toVector)

  /** The header lines, in the order they arrived and with the spelling they arrived in.
    *
    * Neither is tidied up here. [[io.taig.otter.http.codec.HeadersDecoder]] respells incoming names to the schema's and
    * groups a name given more than once, and it has to be the one to do it: it is the side that knows which names the
    * schema declares.
    */
  def toHeaders(headers: Http4sHeaders): Chain[(String, String)] =
    Chain.fromSeq(headers.headers).map(header => (header.name.toString, header.value))

  def toHttp4sHeaders(headers: Chain[(String, String)]): Http4sHeaders =
    Http4sHeaders(headers.toList.map((name, value) => Http4sHeader.Raw(CIString(name), value)))

  /** The essence of a `Content-Type` line, which is all a body is chosen by.
    *
    * Parsed here rather than through http4s's own `MediaType`, and only as far as the `/`. The parameters a media type
    * carries say how bytes became text and where a part ends -- real, but not what tells two bodies apart, and
    * [[Http4sBodyDecoder]] compares on `essence` for exactly that reason. Folding the case with `Locale.ROOT` for the
    * reason [[io.taig.otter.http.codec.HeadersDecoder]] does: a machine in Turkey should not read `TEXT/PLAIN`
    * differently.
    */
  def toMediaType(value: String): Option[MediaType] =
    value.takeWhile(_ != ';').trim.split('/') match
      case Array(primary, secondary) =>
        MediaType(primary.trim.toLowerCase(Locale.ROOT), secondary.trim.toLowerCase(Locale.ROOT)).some
      case _ => none

  /** The `Content-Type` of a message, if it named one it could read. */
  def toMediaType(headers: Http4sHeaders): Option[MediaType] =
    Http4sEnvelope
      .toHeaders(headers)
      .collectFirst { case (name, value) if name.toLowerCase(Locale.ROOT) == "content-type" => value }
      .flatMap(Http4sEnvelope.toMediaType)

  /** The bytes of an entity, read only when there is something that wants them.
    *
    * `Entity.Strict` and `Entity.Empty` are `Entity[Pure]` and already hold everything they have, so neither is a read
    * at all -- which is the whole reason this interpreter can promise that nothing is buffered before a route has
    * matched. Only `Entity.Streamed` costs anything, and reading one whole is what a body described as `Whole` is.
    */
  def toBytes[F[_]: Concurrent](entity: Entity[F]): F[ByteVector] = entity match
    case Entity.Strict(bytes)     => bytes.pure
    case Entity.Empty             => ByteVector.empty.pure
    case Entity.Streamed(body, _) => body.compile.to(Array).map(ByteVector.view)

  /** A response as http4s sends it.
    *
    * The `Content-Type` is written as a raw line from what the body says it is, rather than through http4s's parsed
    * header. [[MediaType.render]] is already the wire form, and going through a second model only adds a way for the
    * two spellings to disagree.
    */
  def toHttp4sResponse[F[_]](response: Http4sWire.Response): ParseResult[Http4sResponse[F]] =
    Status.fromInt(response.code.value).map { status =>
      val headers =
        response.headers ++ Chain.fromOption(response.body.map((mediaType, _) => ("Content-Type", mediaType.render)))

      Http4sResponse[F](
        status = status,
        headers = Http4sEnvelope.toHttp4sHeaders(headers),
        entity = response.body.fold(Entity.empty)((_, bytes) => Entity.strict(bytes))
      )
    }
