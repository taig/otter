package io.taig.otter.http

import io.taig.otter.Keys
import io.taig.otter.http.component.HttpComponent.*
import zio.Scope
import zio.test.*

object EndpointErrorsTest extends ZIOSpecDefault:
  override def spec: Spec[TestEnvironment & Scope, Any] = suite("EndpointErrorsTest")(
    test("overrides preserve annotations before and after attachment and when composing"):
      val domain = endpoint(request(method.get, __ / "health"), response(status.noContent)).attr(Keys.name, "before")
      val declared = domain.withErrors(ErrorOverrides()).attr(Keys.name, "after")
      val expected = domain.attr(Keys.name, "after")
      assertTrue(
        declared.domain.self.metadata == expected.self.metadata,
        declared.effective.self.metadata == expected.self.metadata,
        declared.compose(ErrorPolicy.default).effective.self.metadata == expected.self.metadata,
        declared.compose(ErrorPolicy.default).errors == ErrorPolicy.default,
        domain.effective == domain
      )
    ,
    test("the default unrouted answers are a not found and a method not allowed, and nothing else"):
      assertTrue(
        Responses.branches(UnroutedPolicy.default.responses).map(_.status).toList == List(Status(404), Status(405))
      )
  )
