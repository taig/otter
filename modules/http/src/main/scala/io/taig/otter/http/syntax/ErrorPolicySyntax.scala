package io.taig.otter.http.syntax

import io.taig.otter.http.ComposedEndpoint
import io.taig.otter.http.Endpoint
import io.taig.otter.http.ErrorOverrides
import io.taig.otter.http.ErrorPolicy
import io.taig.otter.http.Failure
import io.taig.otter.http.Status
import io.taig.otter.http.component.ErrorPolicyComponent

/** Construction shortcuts and standalone resolution for declared execution errors. */
trait ErrorPolicySyntax:
  extension [S[-_, +_], E](policy: ErrorPolicy[S, E])
    def apply[T[-w, +r] >: S[w, r], AW, AR, BW, BR](
        endpoint: Endpoint.Schema[T, AW, AR, BW, BR]
    ): ComposedEndpoint[T, AW, AR, BW, BR, E] = ComposedEndpoint(endpoint, policy)

  extension [S[-_, +_], AW, AR, BW, BR](endpoint: Endpoint.Schema[S, AW, AR, BW, BR])
    def withErrors[T[-w, +r] >: S[w, r], E](
        overrides: ErrorOverrides[T, E]
    ): Endpoint.WithErrors[T, AW, AR, BW, BR, E] = Endpoint.WithErrors(endpoint, overrides)

    /** A plain endpoint's standalone contract contains only its domain responses. */
    def effective: Endpoint.Schema[S, AW, AR, BW, BR] = endpoint

  extension [S[-_, +_], AW, AR, BW, BR, E](endpoint: Endpoint.WithErrors[S, AW, AR, BW, BR, E])
    /** Standalone overrides inherit the bodyless defaults. */
    def effective: Endpoint.Schema[S, AW, AR, Either[Failure, BW], Either[E | Status, BR]] =
      endpoint.compose(ErrorPolicyComponent.default).effective

  extension (endpoint: Endpoint.Declaration.Node)
    /** Resolve an existential declaration for standalone documentation. */
    def effective: Endpoint.Node = endpoint match
      case endpoint: Endpoint.Schema[s, ?, ?, ?, ?]        => endpoint
      case endpoint: Endpoint.WithErrors[s, ?, ?, ?, ?, ?] =>
        endpoint.compose(ErrorPolicyComponent.default).effective

object ErrorPolicySyntax extends ErrorPolicySyntax
