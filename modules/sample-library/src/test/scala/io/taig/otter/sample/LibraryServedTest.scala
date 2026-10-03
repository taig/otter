package io.taig.otter.sample

import cats.data.Chain
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import io.taig.otter.http.Endpoint
import io.taig.otter.sample.api.api
import zio.Scope
import zio.Task
import zio.ZIO
import zio.test.*

/** What [[LibraryRoutes]] serves is what [[api.served]] says it serves.
  *
  * The declarations are written without a handler in sight, so documents can be generated from them alone, and the
  * routes are written beside the handlers. Nothing but this test ties the two lists together. A route carries the very
  * declaration it was built from, so the comparison is one of identity and fails on a route that is missing, extra, or
  * registered in a different order.
  */
object LibraryServedTest extends ZIOSpecDefault:
  private val declarations: Task[Chain[Endpoint.Declaration.Node]] =
    ZIO.fromFuture(_ => Library[IO]().map(library => LibraryRoutes.routes(library).declarations).unsafeToFuture())

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("LibraryServedTest")(
    test("the routes serve exactly the served declarations, in order"):
      declarations.map(declarations => assertTrue(declarations == api.served))
    ,
    test("and none of the unserved ones"):
      declarations.map(declarations => assertTrue(api.unserved.toList.intersect(declarations.toList).isEmpty))
  )
