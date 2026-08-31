package io.taig.otter.component

import cats.arrow.Profunctor
import io.taig.otter.Constraint
import io.taig.otter.operation.PrimitiveOperation
import io.taig.validation.Validation
import org.typelevel.ci.CIString

/** `CIString` carried as text.
  *
  * Format agnostic: it asks only for [[io.taig.otter.operation.PrimitiveOperation.Text]] and a `Profunctor`, so it
  * binds to any format whose text primitive is a [[io.taig.otter.Wrapper.Primitive.Text]]. Mix it in next to a format's
  * own vocabulary, where the instances are found in the schema's companion:
  *
  * ```scala
  * object json extends JsonComponent, CaseInsensitiveComponent[Json.Primitive.Text.Schema]
  *
  * json.field("header", json.ciString)
  * ```
  *
  * The schema is the text primitive with a conversion on top, so the validation is stated against `CIString` and
  * contramapped into the node rather than dropped. That is a real conversion and not a refinement, so it is a `dimap`
  * and the node does carry it.
  *
  * To refine it, hand it to [[IronComponent.Text.text]] and bring [[io.taig.validation.cistring]]'s instances into
  * scope for the derivation:
  *
  * ```scala
  * import io.taig.validation.cistring.given
  *
  * json.refined.text[Match[Email.Pattern] & MaxLength[64]](json.ciString)
  * ```
  *
  * Note that `Matches[CIString]` runs the pattern against the underlying string, so a refined `CIString` matches case
  * *sensitively* -- the carrier decides equality, the pattern decides case.
  */
trait CaseInsensitiveComponent[F[-_, +_]](using F: PrimitiveOperation.Text[F], P: Profunctor[F]):
  def ciString(validation: Validation[Constraint.Primitive.Text, CIString]): F[CIString, CIString] =
    P.dimap(F.string(validation.contramap(CIString.apply)))((_: CIString).toString)(CIString.apply)

  val ciString: F[CIString, CIString] = ciString(Validation.valid)
