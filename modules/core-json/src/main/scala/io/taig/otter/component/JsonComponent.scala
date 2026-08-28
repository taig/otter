package io.taig.otter.component

import io.taig.otter.Json
import io.taig.otter.syntax.AllSyntax

/** The user facing vocabulary for defining JSON schemas.
  *
  * Each component is applied to a node's general form. What a combinator holds is inferred at the call site, so
  * `field("title", string)` is a `Json.Field.Of[Json.Primitive.Text.Of, String, String]` and carries the fact that its
  * child is a text primitive.
  */
trait JsonComponent
    extends AllSyntax,
      PrimitiveComponent.Boolean[Json.Primitive.Boolean.Of],
      PrimitiveComponent.Number[Json.Primitive.Number.Of],
      PrimitiveComponent.Text[Json.Primitive.Text.Of],
      RecordComponent[Json.Record.Of, Json.Field.Of],
      TupleComponent[Json.Tuple.Of]:
  object field extends RecordComponent.Field[Json.Field.Of]

  object branch extends BranchComponent[Json.Branch.Of]

  object collection extends CollectionComponent[Json.Collection.Of]

  object dictionary extends DictionaryComponent[Json.Dictionary.Of]

  object constant extends ConstantComponent[Json.Constant.Of]

  object enumeration extends EnumerationComponent[Json.Enumeration.Of]

  object coerce extends CoerceComponent[Json.Coerce.Of]

object JsonComponent extends JsonComponent
