package io.taig.otter.http

import cats.{Eq, Eval}
import cats.syntax.all.*
import cats.data.Chain
import io.taig.otter.http.headers.MediaType
import io.taig.otter.http.headers.ContentType
import io.taig.otter.schema.Collection
import io.taig.otter.schema.{Schema, Value}
import io.taig.otter.validation.Validation
import org.typelevel.ci.CIString

import java.nio.charset.StandardCharsets

object syntax:
  val __ : Url[Unit] = Url.Root

  object header:
    def apply[A](name: CIString, schema: => Value[A] | Collection.Of[Value, A]): Header[A] =
      Header(name, Eval.later(schema))

  def parameter[A](name: String, schema: => Value[A]): Segment[A] = Segment.Parameter(name, Eval.later(schema))

  def query[A](name: String, schema: => Value[A]): Query[A] = Query(name, Eval.later(schema))

  object method:
    val delete: Method = Method("DELETE")
    val get: Method = Method("GET")
    val head: Method = Method("HEAD")
    val patch: Method = Method("PATCH")
    val post: Method = Method("POST")
    val put: Method = Method("PUT")

  object code:
    val ok: Code = Code(200)
    val created: Code = Code(201)
    val accepted: Code = Code(202)
    val noContent: Code = Code(204)
    val movedPermanently: Code = Code(301)
    val found: Code = Code(302)
    val seeOther: Code = Code(303)
    val temporaryRedirect: Code = Code(307)
    val permanentRedirect: Code = Code(308)
    val badRequest: Code = Code(400)
    val unauthorized: Code = Code(401)
    val forbidden: Code = Code(403)
    val notFound: Code = Code(404)
    val conflict: Code = Code(409)
    val payloadTooLarge: Code = Code(413)
    val unprocessableEntity: Code = Code(422)
    val internalServerError: Code = Code(500)
    val serviceUnavailable: Code = Code(503)

  object request:
    val empty: Request.Body.Singlepart.Strict[Unit] = Request.Body.Singlepart.Strict.Empty
    val binary: Request.Body.Singlepart.Strict[Array[Byte]] = Request.Body.Singlepart.Strict.Bytes
    val text: Request.Body.Singlepart.Strict[String] = binary.imapWithHeaders { (headers, bytes) =>
      val charset = headers
        .getFirst(ci"Content-Type")
        .flatMap(ContentType.parse)
        .flatMap(_.charset)
        .flatMap { charset =>
          try Charset.forName(charset).some
          catch {
            case _: IllegalCharsetNameException | _: UnsupportedCharsetException => none
          }
        }
        .getOrElse(StandardCharsets.UTF_8)
      new String(bytes, charset)
    } { value =>
      (
        Http.Headers.one(ci"Content-Type", ContentType(MediaType.text.plain, "utf-8".some).render),
        value.getBytes(StandardCharsets.UTF_8)
      )
    }

//    val streaming: Request.Body.Singlepart.Streaming[Stream] = ???
//
//    object multipart:
//      val empty: Request.Body.Multipart[Unit] = ???
//      def apply[A](part: Request.Body.Multipart.Part[A]): Request.Body.Multipart[A] = ???

  extension [A: Eq, B](self: Chain[(A, B)])
    def all(key: A): Chain[B] = self.collect { case (reference, value) if key === reference => value }
    def first(key: A): Option[B] = self.collectFirst { case (reference, value) if key === reference => value }
    def removeAll(key: A): Chain[(A, B)] = self.filter:
      case (reference, _) if key === reference => false
      case _                                   => true
    def removeFirst(key: A): Chain[(A, B)] =
      var removed = false
      val result = List.newBuilder[(A, B)]
      self.iterator.foreach {
        case (reference, _) if key == reference && !removed => removed = true; ()
        case entry                                          => result += entry
      }
      Chain.fromSeq(result.result())
    def allWithRemainders(key: A): (Chain[B], Chain[(A, B)]) = (all(key), removeAll(key))
    def firstWithRemainders(key: A): Option[(B, Chain[(A, B)])] = first(key).tupleRight(removeFirst(key))
