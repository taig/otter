package io.taig.otter.http.codec

import cats.data.Chain
import io.taig.otter.Keys
import io.taig.otter.http.Code
import io.taig.otter.http.Endpoint
import io.taig.otter.http.MediaType
import io.taig.otter.http.Method
import io.taig.otter.http.TypescriptIssue
import io.taig.otter.http.TypescriptModule
import io.taig.otter.http.fixture.dsl.*
import io.taig.otter.http.fixture.payload
import zio.Scope
import zio.test.*

object TypescriptResponseAlternativesTest extends ZIOSpecDefault:
  private val renderer = TypescriptEndpointRenderer.client(TypescriptEffectPayload.json)
  private val requestSchema = request(Method.Get, __ / "alternatives")
  private val integer = payload.int.attr(Keys.name, "Integer")
  private val text = payload.string.attr(Keys.name, "Text")

  final private case class Unknown[-W, +R]()

  private def render(value: Endpoint.Node): TypescriptModule = renderer.render(Chain.one(value))

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("TypescriptResponseAlternativesTest")(
    test("one status retains all media types"):
      val module = render(
        endpoint(
          requestSchema,
          result(Code.Ok).body(body.json(integer)) :+ result(Code.Ok).body(body(MediaType.Text, text))
        )
      )
      val source = module.render
      assertTrue(
        module.issues.isEmpty,
        source.sliding(6).count(_ == "\"200\":") == 1,
        source.contains("\"application/json\": Integer"),
        source.contains("\"text/plain\": Text")
      )
    ,
    test("one media type retains all schemas in an Effect union"):
      val module =
        render(
          endpoint(requestSchema, result(Code.Ok).body(body.json(integer)) :+ result(Code.Ok).body(body.json(text)))
        )
      val source = module.render
      assertTrue(
        module.issues.isEmpty,
        source.sliding(6).count(_ == "\"200\":") == 1,
        source.contains("\"application/json\": Schema.Union(Integer, Text)"),
        source.contains("Schema.Schema.Type<typeof Integer>"),
        source.contains("Schema.Schema.Type<typeof Text>"),
        source.contains("Schema.Schema.Encoded<typeof Integer>"),
        source.contains("Schema.Schema.Encoded<typeof Text>")
      )
    ,
    test("repeating a schema does not add a union"):
      val source = render(
        endpoint(requestSchema, result(Code.Ok).body(body.json(integer)) :+ result(Code.Ok).body(body.json(integer)))
      ).render
      assertTrue(source.contains("\"application/json\": Integer"), !source.contains("Schema.Union(Integer, Integer)"))
    ,
    test("body alternatives within one result use the same media grouping"):
      val module =
        render(endpoint(requestSchema, result(Code.Ok).bodies(body.json(integer) :+ body.json(text)).toUnion))
      assertTrue(module.issues.isEmpty, module.render.contains("\"application/json\": Schema.Union(Integer, Text)"))
    ,
    test("an undescribed alternative cannot leave a validator for only the known subset"):
      val module = render(
        endpoint(
          requestSchema,
          result(Code.Ok).body(body.json(integer)) :+ result(Code.Ok).body(
            body(MediaType.Json, Unknown[String, String]())
          )
        )
      )
      assertTrue(
        module.issues.contains(TypescriptIssue.Undescribed("GET /alternatives", "application/json")),
        module.render.contains("\"application/json\": undefined")
      )
    ,
    test("an empty alternative remains present in the output union"):
      val source = render(endpoint(requestSchema, result(Code.Ok) :+ result(Code.Ok).body(body.json(integer)))).render
      assertTrue(
        source.contains("{ \"status\": 200 }"),
        source.contains("\"body\": Schema.Schema.Type<typeof Integer>"),
        source.sliding(6).count(_ == "\"200\":") == 1
      )
  )
