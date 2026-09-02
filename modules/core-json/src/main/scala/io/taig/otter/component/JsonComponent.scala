package io.taig.otter.component

import io.taig.otter.Json
import io.taig.otter.codec.JsonTextEncoder
import io.taig.otter.syntax.AllSyntax
import io.taig.otter.syntax.JsonSyntax

/** The user facing vocabulary for defining JSON schemas.
  *
  * Each component is applied to a node's general form. What a combinator holds is inferred at the call site, so
  * `field("title", string)` is a `Json.Field.Of[Json.Primitive.Text.Schema, String, String]` and carries the fact that
  * its child is a text primitive.
  */
trait JsonComponent
    extends AllSyntax,
      JsonSyntax,
      PrimitiveComponent.Boolean[Json.Primitive.Boolean.Schema],
      PrimitiveComponent.Number[Json.Primitive.Number.Schema],
      PrimitiveComponent.Text[Json.Primitive.Text.Schema],
      RecordComponent[Json.Node, Json.Record.Schema, Json.Field.Schema],
      TupleComponent[Json.Node, Json.Tuple.Schema]:
  object field
      extends RecordComponent.Field[Json.Node, Json.Primitive.Text.Node, Json.Field.Schema](using JsonTextEncoder)

  object branch extends BranchComponent[Json.Node, Json.Primitive.Text.Node, Json.Branch.Schema](using JsonTextEncoder)

  object collection extends CollectionComponent[Json.Node, Json.Collection.Schema]

  object dictionary extends DictionaryComponent[Json.Node, Json.Primitive.Text.Node, Json.Dictionary.Schema]

  object constant extends ConstantComponent[Json.Primitive.Node, Json.Constant.Schema]

  object enumeration extends EnumerationComponent[Json.Primitive.Node, Json.Enumeration.Schema]

  object coerce extends CoerceComponent[Json.Primitive.Node, Json.Coerce.Schema]

object JsonComponent extends JsonComponent
