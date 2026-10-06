package io.taig.otter.component

import io.taig.data.Data
import io.taig.otter.Constraint
import io.taig.otter.Json
import io.taig.otter.JsonDiscriminator
import io.taig.otter.codec.JsonTextEncoder
import io.taig.otter.syntax.AllSyntax
import io.taig.otter.syntax.JsonSyntax
import io.taig.validation.Validation

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

  object branch extends BranchComponent[Json.Node, Json.Primitive.Text.Node, Json.Branch.Schema](using JsonTextEncoder):
    def nested[W, R](
        name: String,
        schema: => Json.Node[W, R],
        discriminator: JsonDiscriminator.Nested = JsonDiscriminator.Nested()
    ): Json.Branch.Schema[Json.Record.Node, W, R] = JsonDiscriminator.nested(name, schema, discriminator)

    def merged[W, R](
        name: String,
        schema: => Json.Record.Node[W, R],
        discriminator: JsonDiscriminator.Merged = JsonDiscriminator.Merged()
    ): Json.Branch.Schema[Json.Record.Node, W, R] = JsonDiscriminator.merged(name, schema, discriminator)

  object collection extends CollectionComponent[Json.Node, Json.Collection.Schema]

  object dictionary extends DictionaryComponent[Json.Node, Json.Primitive.Text.Node, Json.Dictionary.Schema]

  object constant extends ConstantComponent[Json.Primitive.Node, Json.Constant.Schema]

  object enumeration extends EnumerationComponent[Json.Primitive.Node, Json.Enumeration.Schema]

  object coerce extends CoerceComponent[Json.Primitive.Node, Json.Coerce.Schema]

  /** Schemas for JSON documents whose shape is supplied at runtime.
    *
    * `any` admits JSON null as `Data.Null`. Wrapping it in `.nullable` instead consumes null as `None`, while a
    * missing-only optional field keeps a present null as `Some(Data.Null)`. Numeric values are read without narrowing
    * decimals or large integers to floating point; JSON spelling and decimal scale are not retained.
    */
  object dynamic:
    def any: Json.Dynamic.Schema[Data, Data] = Json.Dynamic.Schema(Json.Dynamic.Node.AnyValue())

    /** An arbitrary object. Supply an object-count validation to constrain its number of members. */
    def obj: Json.Dynamic.Schema[Data.Object[Data], Data.Object[Data]] = obj(Validation.valid)

    def obj(
        validation: Validation[Constraint.Object, Data.Object[Data]]
    ): Json.Dynamic.Schema[Data.Object[Data], Data.Object[Data]] =
      Json.Dynamic.Schema(Json.Dynamic.Node.ObjectValue(validation))

    def array: Json.Dynamic.Schema[Data.Array[Data], Data.Array[Data]] =
      Json.Dynamic.Schema(Json.Dynamic.Node.ArrayValue)

object JsonComponent extends JsonComponent
