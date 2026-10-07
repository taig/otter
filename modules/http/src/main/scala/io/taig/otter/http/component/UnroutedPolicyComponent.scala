package io.taig.otter.http.component

import io.taig.otter.http.Body
import io.taig.otter.http.Response
import io.taig.otter.http.Status
import io.taig.otter.http.Unrouted
import io.taig.otter.http.UnroutedPolicy

/** Responses for requests addressed to no endpoint, independently of execution-error policies. */
trait UnroutedPolicyComponent:
  /** Bodyless 404 and 405 answers, usable with every payload interpreter. */
  def default: UnroutedPolicy[Nothing] = UnroutedPolicyComponent.bodyless

  def apply[S1[-_, +_], S2[-_, +_]](
      notFound: Response.Writer.Of[S1, Unrouted.NotFound] = default.notFound,
      methodNotAllowed: Response.Writer.Of[S2, Unrouted.MethodNotAllowed] = default.methodNotAllowed
  ): UnroutedPolicy[Body.Or[S1, S2]] = UnroutedPolicy(notFound = notFound, methodNotAllowed = methodNotAllowed)

object UnroutedPolicyComponent extends UnroutedPolicyComponent:
  private val bodyless: UnroutedPolicy[Nothing] =
    def response[A](code: Int): Response.Writer.Of[Nothing, A] =
      Response.Schema(Response.Value.Modify(Response.Value.Root(Status(code)), _ => (), (_: A) => ()))
    UnroutedPolicy(notFound = response(404), methodNotAllowed = response(405))
