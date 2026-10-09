package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.Keys
import io.taig.otter.http.component.HttpComponent.*
import zio.Scope
import zio.test.*

object EndpointErrorsTest extends ZIOSpecDefault:
  override def spec: Spec[TestEnvironment & Scope, Any] = suite("EndpointErrorsTest")(
    test("overrides preserve annotations before and after attachment and when composing"):
      val domain = endpoint(request(method.get, __ / "health"), response(status.noContent)).attr(Keys.name, "before")
      val declared = domain.withErrors(errorOverrides()).attr(Keys.name, "after")
      val expected = domain.attr(Keys.name, "after")
      assertTrue(
        declared.domain.self.metadata == expected.self.metadata,
        declared.effective.self.metadata == expected.self.metadata,
        declared.compose(errorPolicy.default).effective.self.metadata == expected.self.metadata,
        declared.compose(errorPolicy.default).errors == errorPolicy.default,
        domain.effective == domain
      )
    ,
    test("a composed declaration retains every error category under different defaults"):
      def answer(code: Int): Response.Schema[Nothing, Failure, Status] =
        response(Status(code)).dimap[Failure, Status](_ => ())(_ => Status(code))
      val policy = ErrorPolicy(
        envelope = answer(400),
        syntax = answer(401),
        contentType = answer(402),
        validation = answer(403),
        entityRead = answer(404),
        encoding = answer(405),
        status = answer(406),
        unexpected = answer(407)
      )
      val domain = endpoint(request(method.get, __), response(status.noContent))
      val declaration = policy(domain).declaration
      val recomposed = declaration.compose(errorPolicy.from(answer(503)).default)
      assertTrue(
        recomposed.domain == domain,
        recomposed.errors == policy,
        Responses.branches(recomposed.errors.responses).map(_.status).toList == (400 to 407).map(Status(_)).toList
      )
    ,
    test("the default unrouted answers are a not found and a method not allowed, and nothing else"):
      assertTrue(
        Responses.branches(unroutedPolicy.default.responses).map(_.status).toList == List(Status(404), Status(405))
      )
    ,
    test("a named unrouted replacement keeps the other bodyless default"):
      val missing = response(Status(410)).contramap[Unrouted.NotFound](_ => ())
      val policy = unroutedPolicy(notFound = missing)
      assertTrue(
        policy.notFound == missing,
        policy.methodNotAllowed == unroutedPolicy.default.methodNotAllowed,
        Responses.branches(policy.responses).map(_.status).toList == List(Status(410), Status(405))
      )
  )
