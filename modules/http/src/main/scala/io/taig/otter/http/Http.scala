package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.*
import io.taig.otter.Discriminator.Explicit
import io.taig.otter.Discriminator.Merged

object Http:
  final case class Header[A](
      value: Collection[Header, A] | Enumeration[Header, A] | Primitive[A] | Record[Header, A] |
        Union.Untagged[Header, A]
  ) extends AnyVal

  object Header:
    type Invariant = CollectionInvariant[Header, Header] & EnumerationInvariant[Header, Header] &
      PrimitiveInvariant[Header] & RecordInvariant[Header, Header] & UnionInvariant[Header, Header]

    implicit val invariant: Any = new CollectionInvariant[Header, Header]
      with EnumerationInvariant[Header, Header]
      with PrimitiveInvariant[Header]
      with RecordInvariant[Header, Header]
      with UnionInvariant[Header, Header] {

      extension [A](self: Header[A]) override def imap[B](f: A => B)(g: B => A): Header[B] = ???

      extension [A](self: Header[A]) override def metadata: Metadata = ???

      extension [A](self: Header[A]) override def modifyMetadata(f: Metadata => Metadata): Header[A] = ???

      override def list[A](
          codec: => Header[A],
          minimum: Option[Int],
          maximum: Option[Int],
          uniqueItems: Boolean
      ): Header[List[A]] = ???

      override def vector[A](
          codec: => Header[A],
          minimum: Option[Int],
          maximum: Option[Int],
          uniqueItems: Boolean
      ): Header[Vector[A]] = ???

      override def empty: Header[Unit] = ???

      override def field[A](name: String, codec: => Header[A]): Header[A] = ???

      extension [A](self: Header[A]) override def isOptional: Boolean = ???

      extension [A](self: Header[A]) override def optional: Header[Option[A]] = ???

      extension [A](self: Header[A]) override def zip[B](codec: Header[B]): Header[(A, B)] = ???

      override def branch[A](name: String, codec: => Header[A]): Header[A] = ???

      extension [A](self: Header[A]) override def discriminator: Option[Discriminator] = ???

      extension [A](self: Header[A]) override def orElse[B](codec: Header[B]): Header[Either[A, B]] = ???

      extension [A](self: Header[A]) override def keyed: Header[A] = ???

      extension [A](self: Header[A]) override def merged(discriminator: Merged): Header[A] = ???

      extension [A](self: Header[A]) override def explicit(discriminator: Explicit): Header[A] = ???
    }

//   type Path = Vector[String]

//   object Path:
//     val Empty: Http.Path = Vector.empty

//   type Queries = Vector[(String, Option[String])]

//   object Queries:
//     val Empty: Http.Queries = Vector.empty

//   final case class Url(path: Http.Path, queries: Http.Queries):
//     def ++(url: Http.Url): Http.Url = Url(path ++ url.path, queries ++ url.queries)

//     override def toString: String = path.mkString_("/", "/", "") +
//       (if queries.isEmpty then ""
//        else
//          queries
//            .map {
//              case (key, Some(value)) => s"$key=$value"
//              case (key, None)        => key
//            }
//            .mkString_("?", "&", ""))

//   object Url:
//     val Empty: Http.Url = Url(Vector.empty, Vector.empty)

//     given Show[Http.Url] = Show.fromToString

//   type Headers = Vector[(CIString, String)]

//   object Headers:
//     val Empty: Http.Headers = Vector.empty

//   final case class Request(method: Method, url: Http.Url, headers: Http.Headers, body: Array[Byte]):
//     def modifyMethod(f: Method => Method): Http.Request = copy(method = f(method))
//     def withMethod(method: Method): Http.Request = modifyMethod(_ => method)

//     def modifyUrl(f: Http.Url => Http.Url): Http.Request = copy(url = f(url))
//     def withUrl(url: Http.Url): Http.Request = modifyUrl(_ => url)

//     def modifyHeaders(f: Http.Headers => Http.Headers): Http.Request = copy(headers = f(headers))
//     def withHeaders(headers: Http.Headers): Http.Request = modifyHeaders(_ => headers)

//   final case class Response(code: Code, headers: Http.Headers, body: Array[Byte]):
//     def modifyCode(f: Code => Code): Http.Response = copy(code = f(code))
//     def withCode(code: Code): Http.Response = modifyCode(_ => code)

//     def modifyHeaders(f: Http.Headers => Http.Headers): Http.Response = copy(headers = f(headers))
//     def withHeaders(headers: Http.Headers): Http.Response = modifyHeaders(_ => headers)
