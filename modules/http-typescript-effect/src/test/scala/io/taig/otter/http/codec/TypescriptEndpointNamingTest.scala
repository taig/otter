package io.taig.otter.http.codec

import cats.data.Chain
import io.taig.otter.Json
import io.taig.otter.Keys
import io.taig.otter.Typescript
import io.taig.otter.http.Endpoint
import io.taig.otter.http.HttpTypescriptKeys
import io.taig.otter.http.TypescriptModule
import io.taig.otter.http.fixture.dsl.*
import io.taig.otter.http.fixture.payload
import zio.Scope
import zio.test.*

object TypescriptEndpointNamingTest extends ZIOSpecDefault:
  private val renderer = TypescriptEndpointRenderer.client(TypescriptEffectPayload.json)

  private def endpointWith(name: String, schema: Json.Node[?, ?]): Endpoint.Node =
    endpoint(request(method.get, __ / "names"), result(code.ok)(body.json(schema)).toUnion)
      .attr(HttpTypescriptKeys.operationId, name)

  private def render(endpoints: Endpoint.Node*): TypescriptModule = renderer.render(Chain.fromSeq(endpoints))

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("TypescriptEndpointNamingTest")(
    test("case-distinct endpoint names keep distinct derived type aliases"):
      val module = render(endpointWith("foo", payload.int), endpointWith("Foo", payload.string))
      val source = module.render

      assertTrue(
        module.issues.isEmpty,
        source.contains("export const foo ="),
        source.contains("export const Foo ="),
        source.contains("export type FooInput ="),
        source.contains("export type FooInput_2 ="),
        source.contains("export type FooOutput_2 ="),
        source.contains("export type FooEncoded_2 ="),
        source.contains("(input: FooInput_2) =>")
      )
    ,
    test("payloads cannot shadow the endpoint or its type aliases"):
      val module = render(
        endpointWith("foo", payload.int.attr(Keys.name, "FooInput")),
        endpointWith("bar", payload.string.attr(Keys.name, "foo"))
      )
      val source = module.render

      assertTrue(
        module.issues.isEmpty,
        source.contains("export const FooInput_2 = Schema.Int;"),
        source.contains("export const foo_2 = Schema.String;"),
        source.contains("\"application/json\": FooInput_2"),
        source.contains("\"application/json\": foo_2")
      )
    ,
    test("earlier payloads and aliases reserve names for later endpoints"):
      val module = render(
        endpointWith("first", payload.int.attr(Keys.name, "foo")),
        endpointWith("foo", payload.string),
        endpointWith("FirstInput", payload.boolean)
      )
      val source = module.render

      assertTrue(
        module.issues.isEmpty,
        source.contains("export const foo_2 ="),
        source.contains("export const FirstInput_2 =")
      )
    ,
    test("invalid, reserved and import names are normalized without dropping endpoints"):
      val module = render(
        endpointWith("not-valid", payload.int),
        endpointWith("not_valid", payload.int),
        endpointWith("class", payload.int),
        endpointWith("Schema", payload.int),
        endpointWith("9lives", payload.int),
        endpointWith("", payload.int)
      )
      val source = module.render

      assertTrue(
        module.issues.isEmpty,
        source.contains("export const not_valid ="),
        source.contains("export const not_valid_2 ="),
        source.contains("export const class_2 ="),
        source.contains("export const Schema_2 ="),
        source.contains("export const _9lives ="),
        source.contains("export const _ =")
      )
    ,
    test("every declaration shares one registry except each intentional schema type/value pair"):
      val module = render(
        endpointWith("foo", payload.coerce(payload.string).attr(Keys.name, "FooEncoded")),
        endpointWith("Foo", payload.string.attr(Keys.name, "Schema")),
        endpointWith("CoerceString", payload.int),
        endpointWith("String", payload.string)
      )
      val constants = module.declarations.collect { case Typescript.Statement.Declaration.Constant(_, name, _, _) =>
        name
      }
      val types = module.declarations.collect { case Typescript.Statement.Declaration.Type(_, name, _) => name }

      assertTrue(
        module.issues.isEmpty,
        constants.distinct == constants,
        types.distinct == types,
        module.render == render(
          endpointWith("foo", payload.coerce(payload.string).attr(Keys.name, "FooEncoded")),
          endpointWith("Foo", payload.string.attr(Keys.name, "Schema")),
          endpointWith("CoerceString", payload.int),
          endpointWith("String", payload.string)
        ).render
      )
  )
