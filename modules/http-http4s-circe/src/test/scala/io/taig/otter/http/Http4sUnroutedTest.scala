package io.taig.otter.http

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import io.taig.otter.http.fixture.dsl.*
import io.taig.otter.http.fixture.payload
import org.http4s.HttpApp
import org.http4s.HttpRoutes
import org.http4s.Method as Http4sMethod
import org.http4s.Request as Http4sRequest
import org.http4s.Response as Http4sResponse
import org.http4s.Uri
import org.http4s.client.Client
import org.http4s.implicits.*
import zio.Scope
import zio.Task
import zio.ZIO
import zio.test.*

/** What is answered to a request no route matched, which only the router can tell apart.
  *
  * A path no route spells is a `404`; a path some route spells under other methods is a `405` carrying `Allow`. After
  * `Http4s.routes` has fallen through, nothing can tell the two apart or name the methods, which is the whole reason
  * `Http4s.app` and `Http4s.fallback` exist.
  */
object Http4sUnroutedTest extends ZIOSpecDefault:
  private val base: Uri = uri"http://otter.test"
  private val cause = new IllegalStateException("unrouted answer failed")

  /** Injects a synchronous exception to verify the fallback's effect boundary. */
  @SuppressWarnings(Array("scalafix:DisableSyntax.throw"))
  private def crash[A]: A = throw cause

  private val one = endpoint(request(method.get, __ / segment("id", int)), response(status.noContent))
  private val remove = endpoint(request(method.delete, __ / segment("id", int)), response(status.noContent))
  private val root = endpoint(request(method.get, __), response(status.noContent))

  private val declared = UnroutedPolicy(
    response(status.notFound)(body.json(payload.string)).dimap[Unrouted.NotFound, String](unrouted =>
      s"nothing at /${unrouted.path.mkString("/")} for ${unrouted.method.name}"
    )(identity),
    response(status.methodNotAllowed)(body.json(payload.string)).dimap[Unrouted.MethodNotAllowed, String](unrouted =>
      s"only ${unrouted.allowed.toChain.toList.map(_.name).mkString(" and ")}"
    )(identity)
  )

  private val api = Api(ErrorPolicy.default, declared)

  private def run[A](value: IO[A]): Task[A] = ZIO.fromFuture(_ => value.unsafeToFuture())

  private def send(app: HttpApp[IO], method: Http4sMethod, uri: Uri): IO[(Int, List[String], String)] =
    app
      .run(Http4sRequest[IO](method = method, uri = uri))
      .flatMap: response =>
        Http4sEnvelope
          .toBytes(response.entity)
          .map(bytes => (response.status.code, Http4sUnroutedTest.allow(response), bytes.decodeUtf8.getOrElse("")))

  private def allow(response: Http4sResponse[IO]): List[String] =
    response.headers.headers.filter(_.name.toString.equalsIgnoreCase("Allow")).map(_.value)

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("Http4sUnroutedTest")(
    suite("without an API")(
      test("a path no route spells is a bodyless not found, and carries no Allow"):
        val app = Http4s.app[IO](Route(one, (_: Int) => IO.unit))(Http4sCirce.Payload)
        run(send(app, Http4sMethod.GET, base / "orders" / "42"))
          .map((code, allow, body) => assertTrue(code == 404, allow.isEmpty, body.isEmpty))
      ,
      test("a path a route spells under another method is a bodyless method not allowed, and names it"):
        val app = Http4s.app[IO](Route(one, (_: Int) => IO.unit))(Http4sCirce.Payload)
        run(send(app, Http4sMethod.DELETE, base / "42"))
          .map((code, allow, body) => assertTrue(code == 405, allow == List("GET"), body.isEmpty))
      ,
      test("a group of routes answers the same way as the routes it holds"):
        val app = Http4s.app[IO](Routes(Route(one, (_: Int) => IO.unit)))(Http4sCirce.Payload)
        run(send(app, Http4sMethod.DELETE, base / "42")).map((code, allow, _) =>
          assertTrue(code == 405, allow == List("GET"))
        )
    ),
    suite("the methods a path takes")(
      test("are listed once each, in the order they were registered"):
        val app = Http4s.app[IO](
          api,
          Route(one, (_: Int) => IO.unit),
          Route(remove, (_: Int) => IO.unit),
          Route(one, (_: Int) => IO.unit),
          Route(root, (_: Unit) => IO.unit)
        )(Http4sCirce.Payload)
        run(send(app, Http4sMethod.PATCH, base / "1"))
          .map((code, allow, body) =>
            assertTrue(code == 405, allow == List("GET, DELETE"), body == "\"only GET and DELETE\"")
          )
      ,
      test("are decided on the arity and the literals, so a value that does not parse is still a method not allowed"):
        val app = Http4s.app[IO](api, Route(one, (_: Int) => IO.unit))(Http4sCirce.Payload)
        run(send(app, Http4sMethod.DELETE, base / "not-a-number"))
          .map((code, allow, _) => assertTrue(code == 405, allow == List("GET")))
      ,
      test("come from what is routed, and never from what the API only documents"):
        val documented = api.copy(endpoints = cats.data.Chain(remove))
        val app = Http4s.app[IO](documented, Route(root, (_: Unit) => IO.unit))(Http4sCirce.Payload)
        run(send(app, Http4sMethod.GET, base / "1"))
          .map((code, allow, _) => assertTrue(code == 404, allow.isEmpty))
      ,
      test("follow the router's shadowing, since a placeholder answers the literal it shadows"):
        val placeholder = endpoint(request(method.get, __ / "x" / segment("id", string)), response(status.ok))
        val literal = endpoint(request(method.get, __ / "x" / "lit"), response(status.noContent))
        val app = Http4s.app[IO](
          api,
          Route(placeholder, (_: String) => IO.unit),
          Route(literal, (_: Unit) => IO.unit)
        )(Http4sCirce.Payload)
        run(
          (send(app, Http4sMethod.GET, base / "x" / "lit"), send(app, Http4sMethod.POST, base / "x" / "lit")).tupled
        ).map((get, post) => assertTrue(get._1 == 200, post._1 == 405, post._2 == List("GET")))
    ),
    suite("an API's declared answers")(
      test("write their bodies and statuses"):
        val app = Http4s.app[IO](api, Route(one, (_: Int) => IO.unit))(Http4sCirce.Payload)
        run((send(app, Http4sMethod.GET, base / "a" / "b"), send(app, Http4sMethod.PUT, base / "1")).tupled)
          .map((missing, refused) =>
            assertTrue(
              missing._1 == 404,
              missing._3 == "\"nothing at /a/b for GET\"",
              refused._1 == 405,
              refused._2 == List("GET"),
              refused._3 == "\"only GET\""
            )
          )
      ,
      test("an Allow the declaration wrote is replaced by the one the router knows"):
        val claimed = response(status.methodNotAllowed)
          .headers(header("allow", string).toRecord)(body.json(payload.string))
          .dimap[Unrouted.MethodNotAllowed, (String, String)](_ => ("POST", "refused"))(identity)
        val app =
          Http4s.app[IO](
            Api(ErrorPolicy.default, declared.copy(methodNotAllowed = claimed)),
            Route(one, (_: Int) => IO.unit)
          )(
            Http4sCirce.Payload
          )
        run(send(app, Http4sMethod.PUT, base / "1")).map((code, allow, _) =>
          assertTrue(code == 405, allow == List("GET"))
        )
      ,
      test("a method not allowed declared as a not found does not reveal the methods"):
        val hidden = declared.copy(methodNotAllowed =
          declared.notFound.lmap[Unrouted.MethodNotAllowed](unrouted =>
            Unrouted.NotFound(unrouted.method, unrouted.path)
          )
        )
        val app = Http4s.app[IO](Api(ErrorPolicy.default, hidden), Route(one, (_: Int) => IO.unit))(Http4sCirce.Payload)
        run(send(app, Http4sMethod.PUT, base / "1")).map((code, allow, _) => assertTrue(code == 404, allow.isEmpty))
      ,
      test("a not found declared as a method not allowed still carries an Allow, and an empty one"):
        val refusing = declared.copy(notFound =
          response(status.methodNotAllowed)(body.json(payload.string)).dimap[Unrouted.NotFound, String](_ => "no")(
            identity
          )
        )
        val app =
          Http4s.app[IO](Api(ErrorPolicy.default, refusing), Route(one, (_: Int) => IO.unit))(Http4sCirce.Payload)
        run(send(app, Http4sMethod.GET, base / "a" / "b"))
          .map((code, allow, _) => assertTrue(code == 405, allow == List("")))
      ,
      test("do not touch a request a route matched and could not decode"):
        val app = Http4s.app[IO](api, Route(one, (_: Int) => IO.unit))(Http4sCirce.Payload)
        run(send(app, Http4sMethod.GET, base / "not-a-number")).map((code, allow, _) =>
          assertTrue(code == 400, allow.isEmpty)
        )
      ,
      test("that fail to write are raised in the effect rather than observed"):
        val broken = declared.copy(notFound =
          response(status.notFound)(body.json(payload.string)).dimap[Unrouted.NotFound, String](_ => crash)(identity)
        )
        run(
          for
            events <- IO.ref(List.empty[Http4sObservation.Event])
            app = Http4s.app[IO](Api(ErrorPolicy.default, broken), Route(one, (_: Int) => IO.unit))(
              Http4sCirce.Payload,
              observation => events.update(_ :+ observation.event)
            )
            answer <- app.run(Http4sRequest[IO](uri = base / "a" / "b")).attempt
            seen <- events.get
          yield assertTrue(answer == Left(cause), seen.isEmpty)
        )
    ),
    suite("falling through")(
      test("is still what the routes do on their own"):
        val routes = Http4s.routes[IO](api, Route(one, (_: Int) => IO.unit))(Http4sCirce.Payload)
        run(
          (
            routes.run(Http4sRequest[IO](uri = base / "a" / "b")).value,
            routes.run(Http4sRequest[IO](method = Http4sMethod.PUT, uri = base / "1")).value
          ).tupled
        ).map((missing, refused) => assertTrue(missing.isEmpty, refused.isEmpty))
      ,
      test("an unrouted request is not observed, because it has no endpoint to be observed against"):
        run(
          for
            events <- IO.ref(List.empty[Http4sObservation.Event])
            app = Http4s.app[IO](api, Route(one, (_: Int) => IO.unit))(
              Http4sCirce.Payload,
              observation => events.update(_ :+ observation.event)
            )
            _ <- send(app, Http4sMethod.PUT, base / "1")
            _ <- send(app, Http4sMethod.GET, base / "a" / "b")
            _ <- send(app, Http4sMethod.GET, base / "not-a-number")
            seen <- events.get
          yield assertTrue(
            seen.size == 1,
            seen.forall {
              case _: Http4sObservation.Event.Failed => true
              case _                                 => false
            }
          )
        )
      ,
      test("the fallback answers behind foreign routes, and knows only the routes it was given"):
        val health = HttpRoutes.of[IO] {
          case request if request.uri.path.renderString == "/health" => IO.pure(Http4sResponse[IO]())
        }
        val served = List(Route(one, (_: Int) => IO.unit))
        val app =
          (health <+> Http4s.routes[IO](api, served*)(Http4sCirce.Payload) <+> Http4s.fallback[IO](api, served*)(
            Http4sCirce.Payload
          )).orNotFound
        run(
          (
            send(app, Http4sMethod.GET, base / "health"),
            send(app, Http4sMethod.PUT, base / "1"),
            send(app, Http4sMethod.GET, base / "a" / "b"),
            send(app, Http4sMethod.POST, base / "elsewhere" / "deeper")
          ).tupled
        ).map((health, refused, missing, foreign) =>
          assertTrue(
            health._1 == 200,
            refused._1 == 405,
            refused._2 == List("GET"),
            refused._3 == "\"only GET\"",
            missing._1 == 404,
            foreign._1 == 404
          )
        )
      ,
      test("a fallback handed routes that were never tried does not refuse the method it would have taken"):
        val app = Http4s.fallback[IO](Route(one, (_: Int) => IO.unit))(Http4sCirce.Payload).orNotFound
        run(send(app, Http4sMethod.GET, base / "1")).map((code, allow, _) => assertTrue(code == 404, allow.isEmpty))
    ),
    test("a client calling an endpoint the server does not route reports a response it does not describe"):
      val app = Http4s.app[IO](api, Route(root, (_: Unit) => IO.unit))(Http4sCirce.Payload)
      run(Http4s.client[IO, Int, Unit](api, one)(Http4sCirce.Payload, base, Client.fromHttpApp(app))(1).attempt)
        .map(answer =>
          assertTrue(answer.left.exists {
            case _: Http4sFailure.Response => true
            case _                         => false
          })
        )
  )
