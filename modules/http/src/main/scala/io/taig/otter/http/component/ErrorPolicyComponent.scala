package io.taig.otter.http.component

import io.taig.otter.http.Body
import io.taig.otter.http.ErrorPolicy
import io.taig.otter.http.Failure
import io.taig.otter.http.Response
import io.taig.otter.http.Status

/** Named execution-error declarations, with bodyless defaults or a shared custom response. */
trait ErrorPolicyComponent extends ErrorPolicyComponent.Builder[Nothing, Status]:
  override def default: ErrorPolicy[Nothing, Status] = ErrorPolicyComponent.bodyless

  /** Use one static response for every category omitted from the named replacements. */
  def from[S[-_, +_], E](response: Response.Schema[S, Failure, E]): ErrorPolicyComponent.Builder[S, E] =
    new ErrorPolicyComponent.Builder[S, E]:
      override val default: ErrorPolicy[S, E] = ErrorPolicy(
        envelope = response,
        syntax = response,
        contentType = response,
        validation = response,
        entityRead = response,
        encoding = response,
        status = response,
        unexpected = response
      )

object ErrorPolicyComponent extends ErrorPolicyComponent:
  private val bodyless: ErrorPolicy[Nothing, Status] =
    def response(code: Int): Response.Schema[Nothing, Failure, Status] =
      Response.Schema(Response.Value.Modify(Response.Value.Root(Status(code)), _ => Status(code), _ => ()))
    ErrorPolicy(
      envelope = response(400),
      syntax = response(400),
      contentType = response(415),
      validation = response(422),
      entityRead = response(500),
      encoding = response(500),
      status = response(500),
      unexpected = response(500)
    )

  trait Builder[S[-_, +_], E]:
    def default: ErrorPolicy[S, E]

    /** Omitted categories retain this builder's defaults, including their decoded type and payload requirements.
      *
      * Each replacement is inferred independently before requirements and result types are unioned. The omission marker
      * keeps the baseline out of argument inference: otherwise a JSON baseline and a binary replacement can widen to
      * `Object` when written together as `from(json)(encoding = binary)`.
      */
    def apply[
        S1[-_, +_],
        E1,
        S2[-_, +_],
        E2,
        S3[-_, +_],
        E3,
        S4[-_, +_],
        E4,
        S5[-_, +_],
        E5,
        S6[-_, +_],
        E6,
        S7[-_, +_],
        E7,
        S8[-_, +_],
        E8
    ](
        envelope: Response.Schema[S1, Failure, E1] | None.type = None,
        syntax: Response.Schema[S2, Failure, E2] | None.type = None,
        contentType: Response.Schema[S3, Failure, E3] | None.type = None,
        validation: Response.Schema[S4, Failure, E4] | None.type = None,
        entityRead: Response.Schema[S5, Failure, E5] | None.type = None,
        encoding: Response.Schema[S6, Failure, E6] | None.type = None,
        status: Response.Schema[S7, Failure, E7] | None.type = None,
        unexpected: Response.Schema[S8, Failure, E8] | None.type = None
    ): ErrorPolicy[
      [w, r] =>> S[w, r] | S1[w, r] | S2[w, r] | S3[w, r] | S4[w, r] | S5[w, r] | S6[w, r] | S7[w, r] | S8[w, r],
      E | E1 | E2 | E3 | E4 | E5 | E6 | E7 | E8
    ] = ErrorPolicy(
      envelope = resolve(envelope, default.envelope),
      syntax = resolve(syntax, default.syntax),
      contentType = resolve(contentType, default.contentType),
      validation = resolve(validation, default.validation),
      entityRead = resolve(entityRead, default.entityRead),
      encoding = resolve(encoding, default.encoding),
      status = resolve(status, default.status),
      unexpected = resolve(unexpected, default.unexpected)
    )

    private def resolve[T[-_, +_], F](
        value: Response.Schema[T, Failure, F] | None.type,
        fallback: Response.Schema[S, Failure, E]
    ): Response.Schema[Body.Or[S, T], Failure, E | F] = value match
      case None                            => fallback
      case value: Response.Schema[t, ?, ?] => value
