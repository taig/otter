package io.taig.otter.http.codec

import cats.data.Chain
import io.taig.otter.Json
import io.taig.otter.Keys
import io.taig.otter.http.Body
import io.taig.otter.http.Endpoint
import io.taig.otter.http.HttpTypescriptKeys
import io.taig.otter.http.TypescriptIssue
import io.taig.otter.http.TypescriptModule
import io.taig.otter.http.fixture.Report
import io.taig.otter.http.fixture.Settings
import io.taig.otter.http.fixture.api
import io.taig.otter.http.fixture.dsl.*
import zio.Scope
import zio.test.*

/** What a generated descriptor says about the endpoints it came from.
  *
  * Asserted a claim at a time rather than against one golden blob, so that a failure says which claim broke. The claims
  * that matter most are the ones the whole design rests on: that nothing is fetched, that nothing is decoded, and that
  * a caller is handed both the decoded and the encoded shape of an answer so they can put a cache between the two.
  */
object TypescriptEndpointRendererTest extends ZIOSpecDefault:
  private val renderer = TypescriptEndpointRenderer.client(TypescriptEffectPayload.json)

  private def render(endpoints: Endpoint.Node*): TypescriptModule = renderer.render(Chain.fromSeq(endpoints))

  private def source(endpoints: Endpoint.Node*): String = render(endpoints*).render

  /** A named schema with a defaulted field, which is the shape whose two sides genuinely differ: a writer always
    * produces the member and a reader accepts its absence.
    */
  private val settings: Json.Record[Settings] = api.settings.attr(Keys.name, "Settings")

  private val send: Endpoint.Server[Body.Payload, Settings, Unit] =
    endpoint(
      request(method.put, __ :* segment("settings"))(body.json(settings)),
      result(code.noContent).toUnion
    )

  private val answer: Endpoint.Server[Body.Payload, Unit, Settings] =
    endpoint(request(method.get, __ :* segment("settings")), result(code.ok)(body.json(settings)).toUnion)

  /** A second endpoint answering with the same schema, so the name is reached at the read side twice. */
  private val answerAgain: Endpoint.Server[Body.Payload, Unit, Settings] =
    endpoint(request(method.get, __ :* segment("defaults")), result(code.ok)(body.json(settings)).toUnion)

  /** The same pairing over a schema with no such member, which the two sides agree about. */
  private val sendReport: Endpoint.Server[Body.Payload, Report, Unit] =
    endpoint(
      request(method.put, __ :* segment("reports"))(body.json(api.named)),
      result(code.noContent).toUnion
    )

  private val answerReport: Endpoint.Server[Body.Payload, Unit, Report] =
    endpoint(request(method.get, __ :* segment("reports")), result(code.ok)(body.json(api.named)).toUnion)

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("TypescriptEndpointRendererTest")(
    suite("directional names")(
      test("a schema whose two sides differ gets separate definitions"):
        val module = render(send, answer)
        val rendered = module.render

        assertTrue(
          module.issues.isEmpty,
          rendered.contains("export const Settings ="),
          rendered.contains("export const Settings_2 ="),
          rendered.contains("Schema.optionalWith"),
          rendered.contains("\"application/json\": Settings_2")
        )
      ,
      test("the same response side reuses its allocated definition"):
        val rendered = source(send, answer, answerAgain)

        assertTrue(
          render(send, answer, answerAgain).issues.isEmpty,
          rendered.sliding("export const Settings_2 =".length).count(_ == "export const Settings_2 =") == 1,
          !rendered.contains("Settings_3")
        )
      ,
      test("a schema the two sides agree about shares its definition"):
        val rendered = source(sendReport, answerReport)

        assertTrue(render(sendReport, answerReport).issues.isEmpty, !rendered.contains("Report_2"))
      ,
      test("a schema used at one side only has no conflict"):
        assertTrue(render(send).issues.isEmpty, render(answer).issues.isEmpty)
    ),
    suite("module")(
      test("different payloads with the same name stay distinct across endpoints"):
        val first = endpoint(
          request(method.get, __ / "first"),
          result(code.ok)(body.json(api.report.attr(Keys.name, "Shared"))).toUnion
        )
        val second = endpoint(
          request(method.get, __ / "second"),
          result(code.ok)(body.json(api.settings.attr(Keys.name, "Shared"))).toUnion
        )
        val module = render(first, second)

        assertTrue(
          module.issues.isEmpty,
          module.render.contains("export const Shared ="),
          module.render.contains("export const Shared_2 ="),
          module.render.contains("\"application/json\": Shared_2"),
          module.render.contains("\"pages\": Schema.Int"),
          module.render.contains("\"theme\": Schema.optionalWith")
        )
      ,
      test("a module imports the Schema it names"):
        assertTrue(source(api.fetch).startsWith("""import { Schema } from "effect";"""))
      ,
      /** The claim the whole module exists for: what is generated describes a call and never makes one. */
      test("nothing generated fetches or decodes"):
        val rendered = source(api.fetch, api.create, api.trees, api.configure)

        assertTrue(
          !rendered.contains("fetch("),
          !rendered.contains("await"),
          !rendered.contains("async"),
          !rendered.contains("decodeUnknown")
        )
      ,
      /** A payload two endpoints share is declared once, which is what threading one context across every endpoint
        * buys. It takes a name to do it: an anonymous schema has nothing to be recognised by the second time.
        */
      test("a named payload two endpoints send is declared once"):
        val rendered = source(api.replace, api.trees)

        assertTrue(rendered.sliding("export const Tree".length).count(_ == "export const Tree") == 1)
      ,
      test("an endpoint is named by what it says, and otherwise by its method and path"):
        val named = api.trees.attr(HttpTypescriptKeys.operationId, "listTrees")

        assertTrue(
          source(api.fetch).contains("export const getReportsId ="),
          source(named).contains("export const listTrees ="),
          !source(named).contains("getTrees")
        )
      ,
      /** A module cannot bind one name twice, so the first wins and the second is reported rather than overwriting it.
        */
      test("two endpoints claiming one name are reported"):
        val module = render(api.trees, api.trees)

        assertTrue(
          module.issues == List(TypescriptIssue.Duplicate("GET /trees", "getTrees")),
          module.render.sliding("export const getTrees".length).count(_ == "export const getTrees") == 1
        )
    ),
    suite("descriptor")(
      test("the method is kept at the word it spells"):
        assertTrue(source(api.fetch).contains("""  "method": "GET","""), source(api.fetch).contains("} as const;"))
      ,
      /** An array and not a joined string: how the pieces are separated, and what they are joined onto, is the
        * caller's, and one handed a string would have to take it apart again.
        */
      test("a path is the pieces it is made of, with its holes read out of the input"):
        assertTrue(source(api.fetch).contains("""  "path": (input: GetReportsIdInput) => [
                                                |    "reports",
                                                |    String(input["path"]["id"])
                                                |  ],""".stripMargin))
      ,
      test("a path with no holes is still a builder, so every descriptor is read the same way"):
        assertTrue(source(api.trees).contains("""  "path": (input: GetTreesInput) => ["trees"],"""))
      ,
      test("a query parameter is written through the schema that describes it"):
        assertTrue(
          source(api.fetch)
            .contains("""  "query": (input: GetReportsIdInput) => ({ "page": String(input["query"]["page"]) }),""")
        )
      ,
      /** Keyed by media type because a body may be offered as more than one, which is what content negotiation is. */
      test("a body names a schema per media type, and is undefined when there is none"):
        assertTrue(
          source(api.configure).contains("""  "body": { "application/json": PutSettingsRequest },"""),
          source(api.fetch).contains("""  "body": undefined,""")
        )
      ,
      /** The one section of an input that may be absent, and the only place `?` appears in a rendered input type. */
      test("a body that need not be sent is an optional field of the input"):
        val module = render(api.amend)

        assertTrue(
          module.render.contains(""""body"?: """),
          source(api.configure).contains("""export type PutSettingsInput = { "body": """)
        )
      ,
      test("the results name a schema per status code"):
        assertTrue(source(api.fetch).contains("""  "results": {
                                                |    "200": { "application/json": GetReportsIdResponse200 },
                                                |    "404": {}
                                                |  }""".stripMargin))
    ),
    suite("types")(
      test("an input is sectioned, so a path and a query parameter of one name cannot collide"):
        assertTrue(source(api.fetch).contains("""export type GetReportsIdInput = {
                                                |  "path": { "id": number };
                                                |  "query": { "page": number };
                                                |};""".stripMargin))
      ,
      /** The reason the generator stops where it does: a caller names their cache at the encoded type and decodes past
        * it, so both shapes have to be sayable and the two have to be told apart.
        */
      test("an answer is given decoded and encoded, discriminated on the status"):
        val rendered = source(api.fetch)

        assertTrue(
          rendered.contains("""export type GetReportsIdOutput = | {
                              |    "status": 200;
                              |    "body": Schema.Schema.Type<typeof GetReportsIdResponse200>;
                              |  }
                              || { "status": 404 };""".stripMargin),
          rendered.contains("""export type GetReportsIdEncoded = | {
                              |    "status": 200;
                              |    "body": Schema.Schema.Encoded<typeof GetReportsIdResponse200>;
                              |  }
                              || { "status": 404 };""".stripMargin)
        )
      ,
      /** A union of one is the thing itself. */
      test("an answer under one status is not written as a choice"):
        assertTrue(source(api.trees).contains("""export type GetTreesOutput = {
                                                |  "status": 200;
                                                |  "body": Schema.Schema.Type<typeof Tree>;
                                                |};""".stripMargin))
      ,
      /** A schema that refers to itself can only be written down as a name, which is what the fixpoint in
        * `JsonStateTypescriptRenderer` produces -- and it reaches this renderer unchanged.
        */
      test("a recursive payload is declared under its name and suspended where it recurs"):
        assertTrue(source(api.trees).contains("""export const Tree: Schema.Schema<Tree> = Schema.Struct({
                                                |  "value": Schema.Int,
                                                |  "children": Schema.Array(Schema.suspend(() => Tree))
                                                |});""".stripMargin))
      ,
      /** An anonymous payload has no name for a type alias to point at, so it is given one derived from the endpoint
        * rather than written where it stands.
        */
      test("an anonymous payload is declared under a name taken from the endpoint"):
        assertTrue(source(api.fetch).contains("export const GetReportsIdResponse200 = Schema.Struct({"))
    ),
    suite("shortfalls")(
      /** Reported rather than half emitted, which is the stand `Http4sIssue` takes for the same two cases. */
      test("a multipart body is reported and the media type still listed"):
        val module = render(api.create)

        assertTrue(
          module.issues == List(TypescriptIssue.Multipart("POST /reports")),
          module.render.contains("""  "body": { "multipart/form-data": undefined },"""),
          module.render.contains("""export type PostReportsInput = { "body": unknown };""")
        )
      ,
      test("a streamed result is reported"):
        val module = render(api.stream)

        assertTrue(
          module.issues == List(TypescriptIssue.Streamed("GET /reports", "application/x-ndjson")),
          module.render.contains("""  "results": { "200": {} }""")
        )
      ,
      /** A payload alphabet nothing recognises is reported and the body still listed, so a document always comes back.
        */
      test("a payload no renderer knows is reported and the body still listed"):
        val module = TypescriptEndpointRenderer.client(TypescriptPayload.Empty).render(Chain.one(api.configure))

        assertTrue(
          module.issues == List(TypescriptIssue.Undescribed("PUT /settings", "application/json")),
          module.render.contains("""  "body": { "application/json": undefined },""")
        )
    ),
    suite("agreement")(
      /** As `OpenApiRendererTest` checks a rendered operation against what the envelope codecs accept, this checks a
        * generated builder against what the codecs actually write. A descriptor whose path had a different number of
        * pieces than the encoder produces would describe an endpoint nobody can call.
        */
      test("the path a builder writes has the pieces the encoder writes"):
        val pieces = source(api.fetch).linesIterator
          .dropWhile(!_.contains(""""path": (input: GetReportsIdInput) => ["""))
          .drop(1)
          .takeWhile(!_.contains("],"))
          .size

        assertTrue(pieces == PathEncoder.encode(api.one, 1).length.toInt)
      ,
      test("every query parameter a builder writes is one the encoder writes"):
        assertTrue(
          QueriesEncoder.encode(api.paging, 2).map(_._1).toList == List("page"),
          source(api.fetch).contains(""""page": String(input["query"]["page"])""")
        )
    )
  )
