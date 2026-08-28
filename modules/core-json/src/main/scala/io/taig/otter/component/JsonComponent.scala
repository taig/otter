package io.taig.otter.component

import io.taig.otter.Json
import io.taig.otter.syntax.AllSyntax

/** The user facing vocabulary for defining JSON schemas. */
trait JsonComponent
    extends AllSyntax,
      PrimitiveComponent.Boolean[Json.Primitive.Boolean.Of],
      PrimitiveComponent.Number[Json.Primitive.Number.Of],
      PrimitiveComponent.Text[Json.Primitive.Text.Of],
      RecordComponent[Json.Record.Of, Json.Field.Of],
      TupleComponent[Json.Tuple.Of, Json.Of]:
  object field extends RecordComponent.Field[Json.Field.Of, Json.Of]

  object branch extends BranchComponent[Json.Branch.Of, Json.Of]

  object collection extends CollectionComponent[Json.Collection.Of, Json.Of]

  object dictionary extends DictionaryComponent[Json.Dictionary.Of, Json.Of]

  object constant extends ConstantComponent[Json.Constant.Of, Json.Primitive.Of]

  object enumeration extends EnumerationComponent[Json.Enumeration.Of, Json.Primitive.Of]

  object coerce extends CoerceComponent[Json.Coerce.Of, Json.Primitive.Of]

object JsonComponent extends JsonComponent
