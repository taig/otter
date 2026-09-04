package io.taig.otter.http

import io.taig.otter.Json
import io.taig.otter.Metadata
import io.taig.otter.http.fixture.*
import zio.Scope
import zio.test.*

/** What a body description says about itself.
  *
  * These are the questions a renderer and a backend both ask, and asking them here is what makes the answers a property
  * of the description rather than of whoever happens to walk it. Every one is answered from a pure value: there is no
  * effect type on this module's classpath to answer them with.
  */
object BodyDescriptionTest extends ZIOSpecDefault:
  private def filename(part: Metadata): Option[String] =
    part.get(Http.Namespace, Metadata.Namespace.Global)(HttpKeys.filename)

  private val parts = api.upload.self.self.fields.map(_.value.self)

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("BodyDescriptionTest")(
    suite("whole")(
      test("carries the media type it was written as"):
        assertTrue(api.reported.mediaType == MediaType.Json)
      ,
      test("reaches its payload, which is a schema of another alphabet entirely"):
        val payload = api.reported.self.self match
          case Body.Value.Whole(_, payload) => Some(payload.value)
          case _                            => None

        assertTrue(payload.isDefined)
    ),
    suite("alternatives")(
      test("are one branch per media type, in the order they were offered"):
        val media = api.negotiated.self.self.branches.map(_.value.mediaType)

        assertTrue(media.toChain.toList == List(MediaType.Json, MediaType.Pdf))
    ),
    suite("multipart")(
      test("is a body whose payload is a set of parts"):
        assertTrue(api.uploaded.mediaType == MediaType.MultipartFormData)
      ,
      test("names its parts in the order they were declared"):
        assertTrue(parts.map(_.self.name).toList == List("report", "attachment"))
      ,
      test("gives every part a body, and so a content type of its own"):
        val media = parts.map(_.self.schema.value.mediaType)

        assertTrue(media.toList == List(MediaType.Json, MediaType.Pdf))
      ,
      test("carries a filename on the part that claims one, and on no other"):
        assertTrue(parts.map(part => filename(part.metadata)).toList == List(None, Some("report.pdf")))
    ),
    suite("streamed")(
      test("says how its elements are framed and what they are written as"):
        assertTrue(api.reports.frame == Frame.Lines) && assertTrue(api.reports.mediaType == MediaType.NdJson)
      ,
      test("reaches the schema of one element, which is what a renderer documents"):
        val element = api.reports.self.self match
          case Body.Value.Streamed(_, _, element) => Some(element.value)

        assertTrue(element.isDefined)
      ,
      /** The ascription is the assertion: as a request holds it, a streamed body round trips `Unit`. What a sequence of
        * its elements is stays the interpreter's word, so nothing here can name one.
        */
      test("contributes nothing to what the request that holds it reads"):
        val held: Body.Of[Json.Node, Unit] = api.reports.body

        assertTrue(held.mediaType == MediaType.NdJson)
    )
  )
