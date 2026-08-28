package io.taig.otter.component

import io.taig.otter.Json
import io.taig.otter.syntax.AllSyntax

/** The user facing vocabulary for defining JSON schemas.
  *
  * Every component is applied to a node's two parameter form and, right after it, that node's one parameter alias. The
  * alias is what the compiler reports for a round tripping schema, so `field("title", string)` infers as
  * `Json.Field[String]` instead of `Json.Field.Of[String, String]`.
  */
trait JsonComponent
    extends AllSyntax,
      PrimitiveComponent.Boolean[Json.Primitive.Boolean.Of, Json.Primitive.Boolean],
      PrimitiveComponent.Number[Json.Primitive.Number.Of, Json.Primitive.Number],
      PrimitiveComponent.Text[
        Json.Primitive.Text.Of,
        Json.Primitive.Text,
        Json.Primitive.Text.Reader,
        Json.Primitive.Text.Writer
      ],
      RecordComponent[Json.Record.Of, Json.Record, Json.Field.Of],
      TupleComponent[Json.Tuple.Of, Json.Tuple, Json.Of]:
  object field extends RecordComponent.Field[Json.Field.Of, Json.Field, Json.Of, Json]

  object branch extends BranchComponent[Json.Branch.Of, Json.Branch, Json.Of, Json]

  object collection extends CollectionComponent[Json.Collection.Of, Json.Collection, Json.Of, Json]

  object dictionary extends DictionaryComponent[Json.Dictionary.Of, Json.Dictionary, Json.Of, Json]

  object constant extends ConstantComponent[Json.Constant.Of, Json.Constant, Json.Primitive.Of, Json.Primitive]

  object enumeration
      extends EnumerationComponent[Json.Enumeration.Of, Json.Enumeration, Json.Primitive.Of, Json.Primitive]

  object coerce extends CoerceComponent[Json.Coerce.Of, Json.Coerce, Json.Primitive.Of, Json.Primitive]

object JsonComponent extends JsonComponent
