package io.taig.otter.sample

import cats.data.Chain
import io.taig.otter.http.Endpoint
import io.taig.otter.http.Http4sCirce
import io.taig.otter.http.OpenApiProfile
import io.taig.otter.http.codec.Http4sResponseDecoder
import io.taig.otter.http.codec.Http4sResponseEncoder
import io.taig.otter.http.codec.OpenApiPayload
import io.taig.otter.http.codec.OpenApiRenderer
import io.taig.otter.http.codec.TypescriptEffectPayload
import io.taig.otter.http.codec.TypescriptEndpointRenderer
import io.taig.otter.sample.api.api
import io.taig.otter.sample.api.books
import io.taig.otter.sample.api.dsl.*
import io.taig.otter.sample.api.schema
import zio.Scope
import zio.test.*

object LibraryDeletionContractTest extends ZIOSpecDefault:
  enum Empty:
    case Removed, Missing, Unchanged

  private val empty = (response(status.noContent).to[LibraryDeletionContractTest.Empty.Removed.type] :+
    response(status.notFound).to[LibraryDeletionContractTest.Empty.Missing.type] :+
    response(status.notModified).to[LibraryDeletionContractTest.Empty.Unchanged.type])
    .to[LibraryDeletionContractTest.Empty]

  private val structural = endpoint(
    request(method.delete, books.one),
    response(status.noContent) :+ response(status.conflict)(body.json(schema.problem))
  ).attr(openapi.operationId, "deleteBook")
    .attr(openapi.tags, "books")

  private val before = Chain.one[Endpoint.Declaration.Node](structural)
  private val after = Chain.one[Endpoint.Declaration.Node](books.delete)

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("LibraryDeletionContractTest")(
    test("every case of an all-bodyless sum writes its status and reads back without an entity"):
      val encoder = new Http4sResponseEncoder(Http4sCirce.Payload)
      val decoder = new Http4sResponseDecoder(Http4sCirce.Payload)
      val cases = List(
        LibraryDeletionContractTest.Empty.Removed -> status.noContent,
        LibraryDeletionContractTest.Empty.Missing -> status.notFound,
        LibraryDeletionContractTest.Empty.Unchanged -> status.notModified
      )
      assertTrue(cases.forall: (value, status) =>
        encoder
          .encode(empty, value)
          .exists: wire =>
            wire.status == status && wire.body.isEmpty && decoder.decode(empty, wire).toOption.contains(value))
    ,
    test("named cases preserve both OpenAPI contracts, metadata, references and issues"):
      val payload = OpenApiPayload.json(OpenApiProfile.V31)
      val renderers = List(
        OpenApiRenderer.server(OpenApiProfile.V31, payload),
        OpenApiRenderer.client(OpenApiProfile.V31, payload)
      )
      assertTrue(renderers.forall: renderer =>
        val expected = renderer.render(api.Info, before)
        val actual = renderer.render(api.Info, after)
        actual == expected && actual.issues.isEmpty &&
        actual.value.noSpaces.contains("#/components/schemas/Problem"))
    ,
    test("named cases preserve both TypeScript contracts, metadata, references and issues"):
      val renderers = List(
        TypescriptEndpointRenderer.server(TypescriptEffectPayload.json),
        TypescriptEndpointRenderer.client(TypescriptEffectPayload.json)
      )
      assertTrue(renderers.forall: renderer =>
        val expected = renderer.render(before)
        val actual = renderer.render(after)
        actual == expected && actual.issues.isEmpty &&
        actual.render.contains("\"204\": {}") &&
        actual.render.contains("\"409\": { \"application/json\": Problem }"))
  )
