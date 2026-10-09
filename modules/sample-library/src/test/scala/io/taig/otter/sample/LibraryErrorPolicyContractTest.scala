package io.taig.otter.sample

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import io.taig.otter.Json
import io.taig.otter.Keys
import io.taig.otter.http.Api
import io.taig.otter.http.Body
import io.taig.otter.http.Failure
import io.taig.otter.http.Http4s
import io.taig.otter.http.Http4sCirce
import io.taig.otter.http.OpenApi
import io.taig.otter.http.OpenApiProfile
import io.taig.otter.http.Response
import io.taig.otter.http.Responses
import io.taig.otter.http.Route
import io.taig.otter.http.Status
import io.taig.otter.http.codec.OpenApiPayload
import io.taig.otter.http.codec.OpenApiRenderer
import io.taig.otter.http.codec.TypescriptEffectPayload
import io.taig.otter.http.codec.TypescriptEndpointRenderer
import io.taig.otter.sample.api.dsl.*
import io.taig.otter.sample.api.json
import org.http4s.Request as Http4sRequest
import org.http4s.implicits.*
import zio.Scope
import zio.ZIO
import zio.test.*

object LibraryErrorPolicyContractTest extends ZIOSpecDefault:
  private val errorSchema = json.string.attr(Keys.name, "PolicyError")

  private def error(code: Int, retry: Int, message: String): Response.Schema[Body.Whole[Json.Node], Failure, String] =
    response(Status(code))
      .headers(header("Retry-After", int).toRecord)(body.json(errorSchema))
      .dimap[Failure, String](_ => (retry, message))(_._2)

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("LibraryErrorPolicyContractTest")(
    List(false, true).map: settled =>
      test(if settled then "composed policy matches both documents" else "local overrides match both documents"):
        val initial = errorPolicy.from(error(502, 1, "initial")).default
        val serving = errorPolicy.from(error(504, 2, "serving")).default
        val declaration = endpoint(
          request(method.get, __ / "policy").queries(query("n", int).toRecord),
          response(status.noContent)
        ).withErrors(errorOverrides(unexpected = Some(error(503, 5, "local"))))
        val handler = (_: Int) => IO.raiseError[Unit](new IllegalStateException("handler"))
        val route =
          if settled then Route.composed(declaration.compose(initial), handler)
          else Route(declaration, handler)
        val api = Api(serving, unroutedPolicy.default, route.declaration)
        val app = Http4s.app[IO](api, route)(Http4sCirce.Payload)
        val inheritedStatus = if settled then 502 else 504
        val excludedStatus = if settled then 504 else 502
        val expectedStatuses = Set(204, inheritedStatus, 503)
        val inheritedMessage = if settled then "initial" else "serving"
        val inheritedRetry = if settled then "1" else "2"
        val info = OpenApi.Info("Error policies", "1")
        val openapi = OpenApiRenderer.server(OpenApiProfile.V31, OpenApiPayload.json(OpenApiProfile.V31))
        val document = openapi.render(info, api)
        val responses =
          document.value.hcursor.downField("paths").downField("/policy").downField("get").downField("responses")
        val typescript = TypescriptEndpointRenderer.client(TypescriptEffectPayload.json)
        val module = typescript.render(api)
        val source = module.render
        val effectiveStatuses =
          api.effective.flatMap(endpoint => Responses.branches(endpoint.responses).map(_.status.value)).toList.toSet
        def exchange(n: String): IO[(Int, String, Option[String])] =
          app
            .run(Http4sRequest[IO](uri = uri"http://otter.test/policy".withQueryParam("n", n)))
            .flatMap: response =>
              response
                .as[String]
                .map: content =>
                  (
                    response.status.code,
                    content,
                    response.headers.headers.find(_.name.toString == "Retry-After").map(_.value)
                  )
        ZIO
          .fromFuture(_ => (exchange("invalid"), exchange("1")).tupled.unsafeToFuture())
          .map: (inherited, local) =>
            assertTrue(
              inherited == (inheritedStatus, s"\"$inheritedMessage\"", Some(inheritedRetry)),
              local == (503, "\"local\"", Some("5")),
              effectiveStatuses == expectedStatuses,
              document.issues.isEmpty,
              document == openapi.render(info, api.effective),
              responses.keys.map(_.toSet).contains(expectedStatuses.map(_.toString)),
              List(inheritedStatus, 503).forall: code =>
                val answer = responses.downField(code.toString)
                answer.downField("content").downField("application/json").downField("schema").get[String]("$ref") ==
                  Right("#/components/schemas/PolicyError") &&
                  answer.downField("headers").downField("Retry-After").downField("schema").get[String]("type") == Right(
                    "integer"
                  )
              ,
              module.issues.isEmpty,
              module == typescript.render(api.effective),
              expectedStatuses.forall(code => source.contains(s"\"$code\":")),
              !source.contains(s"\"$excludedStatus\":"),
              source.contains("\"application/json\": PolicyError"),
              source.contains("Schema.Schema.Type<typeof PolicyError>"),
              source.contains("Schema.Codec.Encoded<typeof PolicyError>")
            )
  )
