// package io.taig.otter.http

// import cats.syntax.all.*
// import io.taig.otter as Base
// import io.taig.otter.http.header.Accept
// import io.taig.otter.http.header.MediaType
// import io.taig.otter.http.header.Parameters
// import org.typelevel.ci.*
// import org.typelevel.ci.CIString

// import java.nio.charset.Charset
// import java.nio.charset.StandardCharsets
// import java.util.regex.Pattern

// trait Codecs extends Base.Codecs, Types:
//   self =>

//   final def cistring(
//       minLength: Option[Int] = none,
//       maxLength: Option[Int] = none,
//       matches: Option[Pattern] = none
//   ): Primitive.Of[String, CIString] = string(minLength, maxLength, matches).imap(CIString.apply)(_.toString)

//   final val cistring: Primitive.Of[String, CIString] = cistring()

//   implicit final class CIStringStringCodecBuilder(codec: cistring.type) extends StringCodecBuilder[CIString]:
//     override def apply(
//         minLength: Option[Int],
//         maxLength: Option[Int],
//         matches: Option[Pattern]
//     ): Primitive.Of[String, CIString] = cistring(minLength, maxLength, matches)
//     override def isEmpty(a: CIString): Boolean = a.isEmpty
//     override def empty: CIString = CIString.empty

//   val __ : Url[Unit] = Url.Empty

//   object method:
//     inline def apply(value: String): Method = Method(value)

//     val delete: Method = method("DELETE")
//     val get: Method = method("GET")
//     val head: Method = method("HEAD")
//     val patch: Method = method("PATCH")
//     val post: Method = method("POST")
//     val put: Method = method("PUT")

//   object mediaType:
//     def apply(primary: String, secondary: String): MediaType =
//       MediaType(tpe = MediaType.Type(primary, secondary), parameters = Parameters.Empty)

//     object application:
//       def apply(secondary: String): MediaType = mediaType(primary = "application", secondary)

//       val json: MediaType = application(secondary = "json")
//       val octetStream: MediaType = application(secondary = "octet-stream")
//       val wwwFormUrlencoded: MediaType = application(secondary = "x-www-form-urlencoded")

//     object multipart:
//       def apply(secondary: String): MediaType = mediaType(primary = "multipart", secondary)

//       val fromData: MediaType = application(secondary = "form-data")

//     object text:
//       def apply(secondary: String): MediaType = mediaType(primary = "text", secondary)

//       val csv: MediaType = text(secondary = "csv")
//       val plain: MediaType = text(secondary = "plain")
//       val html: MediaType = text(secondary = "html")

//   object header:
//     inline def apply[A](
//         name: CIString,
//         codec: Codec.Of[Data.Primitive | Data.Array[Data.Primitive] | Data.Object[Data.Nullable[Data.Primitive]], A]
//     ): Header.Required[A] = inline codec match
//       case codec: Codec.Of[Data.Primitive, A] => Header.Required.Primitive(name, codec, metadata = Metadata.Empty)
//       case codec: Codec.Of[Data.Array[Data.Primitive], A] =>
//         Header.Required.Array(name, codec, metadata = Metadata.Empty, delimiter = Delimiter.Default)
//       case codec: Codec.Of[Data.Object[Data.Nullable[Data.Primitive]], A] =>
//         Header.Required.Object(name, codec, metadata = Metadata.Empty)

//     inline def apply[A](
//         name: CIString,
//         codec: Codec.Of[Data.Nullable[
//           Data.Primitive | Data.Array[Data.Primitive] | Data.Object[Data.Nullable[Data.Primitive]]
//         ], A]
//     ): Header[A] = header.optional(name, codec)

//     inline def optional[A](
//         name: CIString,
//         codec: Codec.Of[Data.Nullable[
//           Data.Primitive | Data.Array[Data.Primitive] | Data.Object[Data.Nullable[Data.Primitive]]
//         ], A]
//     ): Header[A] = inline codec match
//       case codec: Codec.Of[Data.Nullable[Data.Primitive], A] => Header.Primitive(name, codec, metadata = Metadata.Empty)
//       case codec: Codec.Of[Data.Nullable[Data.Array[Data.Primitive]], A] =>
//         Header.Array(name, codec, metadata = Metadata.Empty, delimiter = Delimiter.Default)
//       case codec: Codec.Of[Data.Nullable[Data.Object[Data.Nullable[Data.Primitive]]], A] =>
//         Header.Object(name, codec, metadata = Metadata.Empty)

//     def accept[A](codec: Codec.Of[Data.Primitive, A]): Header.Required[A] = header(ci"Accept", codec)
//     val accept: Header.Required[Accept] = accept(Accept.codec)

//     def authorization[A](codec: Codec.Of[Data.Primitive, A]): Header.Required[A] = header(ci"Authorization", codec)

//     def contentType[A](codec: Codec.Of[Data.Primitive, A]): Header.Required[A] = header(ci"Content-Type", codec)
//     val contentType: Header.Required[MediaType] = contentType(MediaType.codec)

//   final inline def parameter[A](
//       name: String,
//       codec: Codec.Of[Data.Primitive, A]
//   ): Segment.Parameter[A] = Segment.Parameter.Primitive(name, codec, Metadata.Empty)

//   object query:
//     final inline def apply[A](
//         name: String,
//         codec: Codec.Of[Data.Primitive | Data.Array[Data.Primitive] | Data.Object[Data.Nullable[Data.Primitive]], A]
//     ): Query.Required[A] = inline codec match
//       case codec: Codec.Of[Data.Primitive, A] => Query.Required.Primitive(name, codec, metadata = Metadata.Empty)
//       case codec: Codec.Of[Data.Array[Data.Primitive], A] =>
//         Query.Required.Array(name, codec, metadata = Metadata.Empty, delimiter = Delimiter.Default)
//       case codec: Codec.Of[Data.Object[Data.Nullable[Data.Primitive]], A] =>
//         Query.Required.Object(name, codec, metadata = Metadata.Empty)

//     final inline def apply[A](
//         name: String,
//         codec: Codec.Of[Data.Nullable[
//           Data.Primitive | Data.Array[Data.Primitive] | Data.Object[Data.Nullable[Data.Primitive]]
//         ], A]
//     ): Query[A] = query.optional(name, codec)

//     final inline def nullable[A](
//         name: String,
//         codec: Codec.Of[Data.Nullable[Data.Primitive | Data.Array[Data.Primitive]], A]
//     ): Query[A] = inline codec match
//       case codec: Codec.Of[Data.Nullable[Data.Primitive], A] =>
//         Query.Primitive(name, codec, metadata = Metadata.Empty, nullable = true)
//       case codec: Codec.Of[Data.Nullable[Data.Array[Data.Primitive]], A] =>
//         Query.Array(name, codec, metadata = Metadata.Empty, delimiter = Delimiter.Default, nullable = true)

//     final inline def optional[A](
//         name: String,
//         codec: Codec.Of[Data.Nullable[
//           Data.Primitive | Data.Array[Data.Primitive] | Data.Object[Data.Nullable[Data.Primitive]]
//         ], A]
//     ): Query[A] = inline codec match
//       case codec: Codec.Of[Data.Nullable[Data.Primitive], A] =>
//         Query.Primitive(name, codec, metadata = Metadata.Empty, nullable = false)
//       case codec: Codec.Of[Data.Nullable[Data.Array[Data.Primitive]], A] =>
//         Query.Array(name, codec, metadata = Metadata.Empty, delimiter = Delimiter.Default, nullable = false)

//   object body:
//     def apply[A](
//         mediaType: MediaType,
//         f: (Option[Charset], Array[Byte]) => Codec.Result[A],
//         g: (Option[Charset], A) => Array[Byte]
//     ): Body.Strict[A] = Body.Strict(mediaType, of = none, f, g)

//     def apply[O <: Data, A](
//         mediaType: MediaType,
//         codec: Codec.Of[O, A],
//         f: (Option[Charset], Array[Byte]) => Codec.Result[Data],
//         g: (Option[Charset], O) => Array[Byte]
//     ): Body.Strict[A] = Body.Strict(
//       mediaType,
//       of = codec.some,
//       f(_, _).andThen(codec.decode),
//       (charset, o) => g(charset, codec.encode(o))
//     )

//   def binary(mediaType: MediaType): Body.Strict[Array[Byte]] =
//     body(mediaType, (_, bytes) => bytes.valid, (_, bytes) => bytes)
//   val binary: Body.Strict[Array[Byte]] = binary(mediaType.application.octetStream)

//   def text(fallback: => Charset): Body.Strict[String] = body(
//     mediaType = mediaType.text.plain,
//     (charset, bytes) => new String(bytes, charset.getOrElse(fallback)).valid,
//     (charset, text) => text.getBytes(charset.getOrElse(fallback))
//   )

//   val text: Body.Strict[String] = text(fallback = StandardCharsets.UTF_8)

//   def text[A](
//       codec: Codec.Of[Data.Primitive, A],
//       fallback: => Charset = StandardCharsets.UTF_8
//   ): Body.Strict[A] = body(
//     mediaType = mediaType.text.plain,
//     codec,
//     (charset, bytes) =>
//       val value = new String(bytes, charset.getOrElse(fallback))
//       String(value).valid
//     ,
//     (charset, data) => data.plain.getBytes(charset.getOrElse(fallback))
//   )

//   object formData:
//     private type Of = Data.Object[Data.Nullable[Data.Primitive]]

//     def apply[A](codec: Codec.Of[Of, A], fallback: Charset = StandardCharsets.UTF_8): Body[A] = body(
//       mediaType = mediaType.application.wwwFormUrlencoded,
//       codec,
//       (charset, bytes) =>
//         val value = new String(bytes, charset.getOrElse(fallback))
//         val formData = FormData.parse(value).toVector
//         Data.Object(formData.map { case (key, value) => (key, value.fold(Data.Null)(String.apply)) }).valid
//       ,
//       (charset, data) =>
//         val values = data.values.map:
//           case (key, Data.Null)            => (key, none)
//           case (key, data: Data.Primitive) => (key, data.plain.some)

//         FormData(values).show.getBytes(charset.getOrElse(fallback))
//     )

//   def endpoint[A, B](request: Request[A], response: Response[B]): Endpoint[A, B] = Endpoint(request, response)

//   def app[F[_]](routes: Routes[F]): App[F] = App(
//     routes,
//     error = result(code.notFound, text(error.text.routeNotFound)).toResults.to[App.Error]
//   )

//   final def result[A, B](code: Code, headers: Headers[A], bodies: Bodies[B])(using
//       merge: Merge[A, B]
//   ): Result[merge.Out] = Result(code, headers, bodies).imap(merge.apply)(merge.unapply)

//   final def result[A, B](code: Code, headers: Headers[A], body: Body[B])(using
//       merge: Merge[A, B]
//   ): Result[merge.Out] = result(code, headers, body.toBodies)

//   final def result[A](code: Code, bodies: Bodies[A]): Result[A] =
//     Result(code, Headers.Empty, bodies).imap { case (_, a) => a }(a => ((), a))

//   final def result[A](code: Code, body: Body[A]): Result[A] = result(code, body.toBodies)

//   final def result[A](code: Code, headers: Headers[A]): Result[A] = Result(code, headers)

//   final def result(code: Code): Result[Unit] = result(code, Headers.Empty)

//   final def request[A, B, C](method: Method, url: Url[A], headers: Headers[B], bodies: Bodies[C])(using
//       merge1: Merge[A, B],
//       merge2: Merge[merge1.Out, C]
//   ): Request[merge2.Out] = Request(method, url, headers, bodies).imap { case (a, b, c) =>
//     merge2(merge1(a, b), c)
//   } { out =>
//     val (ab, c) = merge2.unapply(out)
//     merge1.unapply(ab) :* c
//   }

//   final def request[A, B, C](method: Method, url: Url[A], headers: Headers[B], body: Body[C])(using
//       merge1: Merge[A, B],
//       merge2: Merge[merge1.Out, C]
//   ): Request[merge2.Out] = ??? // request(method, url, headers, body.toBodies)

//   final def request[A, B](method: Method, url: Url[A], bodies: Bodies[B])(using
//       merge: Merge[A, B]
//   ): Request[merge.Out] = Request(method, url, Headers.Empty, bodies)
//     .imap { case (a, _, b) => merge((a, b)) } { out =>
//       val (a, b) = merge.unapply(out)
//       (a, (), b)
//     }

//   final def request[A, B](method: Method, url: Url[A], body: Body[B])(using
//       merge: Merge[A, B]
//   ): Request[merge.Out] = request(method, url, body.toBodies)

//   final def request[A, B](method: Method, url: Url[A], headers: Headers[B])(using
//       merge: Merge[A, B]
//   ): Request[merge.Out] = Request(method, url, headers).imap(merge.apply)(merge.unapply)

//   final def request[A](method: Method, url: Url[A]): Request[A] =
//     Request(method, url, Headers.Empty).imap { case (a, _) => a }(a => (a, ()))

//   object error:
//     def apply[O <: Data.Value, A](
//         tpe: String,
//         codec: Codec.Of[Data.Nullable[O], A]
//     ): Record.Of[String | O, A] = field("error", constant(tpe)) :* field.optional("value", codec)

//     def apply(tpe: String): Record[Unit] = error(tpe, void)

//     val routeNotFound = error(tpe = "routeNotFound").as(App.Error.RouteNotFound)

//     val contentNegotiationFailed = error(
//       tpe = "contentNegotiationFailed",
//       codec = violations.structured.to[Route.Error.ContentNegotiationFailed]
//     )

//     val mediaTypesUnsupported = error(
//       tpe = "mediaTypesUnsupported",
//       codec = violations.structured.to[Route.Error.MediaTypesUnsupported]
//     )

//     val validationViolations = error(
//       tpe = "validationViolations",
//       codec = violations.structured.to[Route.Error.ValidationViolations]
//     )

//     object text:
//       val routeNotFound: Primitive[App.Error.RouteNotFound.type] =
//         parser(name = "routeNotFound")(App.Error.parse)(_.show)
//       val contentNegotiationFailed: Primitive[Route.Error.ContentNegotiationFailed] =
//         parser(name = "contentNegotiationFailed")(Route.Error.ContentNegotiationFailed.parse)(_.show)
//       val mediaTypesUnsupported: Primitive[Route.Error.MediaTypesUnsupported] =
//         parser(name = "mediaTypesUnsupported")(Route.Error.MediaTypesUnsupported.parse)(_.show)
//       val validationViolations: Primitive[Route.Error.ValidationViolations] =
//         parser(name = "validationViolations")(Route.Error.ValidationViolations.parse)(_.show)

//   final def response[A](results: Results[A], error: Results[Route.Error], failure: Result[Unit]): Response[A] =
//     Response(results, error, failure)

//   def response[A](results: Results[A]): Response[A] = response(
//     results,
//     error = (
//       result(code.notAcceptable, text(error.text.contentNegotiationFailed)) :+
//         result(code.unsupportedMediaTypes, text(error.text.mediaTypesUnsupported)) :+
//         result(code.unprocessableEntity, text(error.text.validationViolations))
//     ).to,
//     failure = result(code.internalServerError)
//   )

//   final def response[A](result: Result[A]): Response[A] = response(result.toResults)

//   final def response[A, B](errors: Results[A], results: Results[B]): Response[Either[A, B]] =
//     response(errors.orElse(results))

//   final def response[A, B](errors: Results[A], result: Result[B]): Response[Either[A, B]] =
//     response(errors :+ result)

//   final def response[A, B](error: Result[A], result: Result[B]): Response[Either[A, B]] =
//     response(error.toResults :+ result)

//   final def response[A, B](error: Result[A], results: Results[B]): Response[Either[A, B]] =
//     response(error.toResults.orElse(results))

//   // Scala.js won't compile if this is included here (for reasons unknown)
//   export ViolationsCodecs.*

// object Codecs extends Codecs
