package io.taig.otter.component

import io.taig.otter.Json
import io.taig.otter.syntax.AllSyntax

/** The user facing vocabulary for defining JSON schemas. */
trait JsonComponent
    extends AllSyntax,
      PrimitiveComponent.Boolean[Json.Primitive.Boolean],
      PrimitiveComponent.Number[Json.Primitive.Number],
      PrimitiveComponent.Text[Json.Primitive.Text],
      RecordComponent[Json.Record, Json.Field],
      TupleComponent[Json.Tuple, Json]:
  object field extends RecordComponent.Field[Json.Field, Json]

  object branch extends BranchComponent[Json.Branch, Json]

  object collection extends CollectionComponent[Json.Collection, Json]

  object dictionary extends DictionaryComponent[Json.Dictionary, Json]

  object constant extends ConstantComponent[Json.Constant, Json.Primitive]

  object coerce extends CoerceComponent[Json.Coerce, Json.Primitive]


object JsonComponent extends JsonComponent
