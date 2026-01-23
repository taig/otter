package io.taig.otter.component

import io.taig.otter.Json

trait JsonComponent
    extends PrimitiveComponent.Boolean[Json.Primitive.Boolean],
      PrimitiveComponent.Coerce.Boolean[Json.Primitive.Coerce.Boolean, Json.Primitive.Boolean],
      PrimitiveComponent.Coerce.Boolean.Read[Json.Primitive.Coerce.Boolean.Read, Json.Primitive.Boolean.Read],
      PrimitiveComponent.Coerce.Boolean.Write[Json.Primitive.Coerce.Boolean.Write, Json.Primitive.Boolean.Write],
      PrimitiveComponent.Coerce.Number[Json.Primitive.Coerce.Number, Json.Primitive.Number],
      PrimitiveComponent.Coerce.Number.Read[Json.Primitive.Coerce.Number.Read, Json.Primitive.Number.Read],
      PrimitiveComponent.Coerce.Number.Write[Json.Primitive.Coerce.Number.Write, Json.Primitive.Number.Write],
      PrimitiveComponent.Coerce.Text[Json.Primitive.Coerce.Text, Json.Primitive.Text],
      PrimitiveComponent.Coerce.Text.Read[Json.Primitive.Coerce.Text.Read, Json.Primitive.Text.Read],
      PrimitiveComponent.Coerce.Text.Write[Json.Primitive.Coerce.Text.Write, Json.Primitive.Text.Write],
      PrimitiveComponent.Number[Json.Primitive.Number],
      PrimitiveComponent.Text[Json.Primitive.Text],
      PrimitiveComponent.Text.Read[Json.Primitive.Text.Read],
      PrimitiveComponent.Text.Write[Json.Primitive.Text.Write],
      RecordComponent[Json.Record, Json.Field],
      TupleComponent[Json.Tuple, Json]:
  object collection
      extends CollectionComponent[Json.Collection, Json],
        CollectionComponent.Read[Json.Collection.Read, Json.Read],
        CollectionComponent.Write[Json.Collection.Write, Json.Write]

  object constant
      extends ConstantComponent[Json.Constant, Json.Primitive],
        ConstantComponent.Write[Json.Constant.Write, Json.Primitive.Write]

  object dictionary
      extends DictionaryComponent[Json.Dictionary, Json],
        DictionaryComponent.Read[Json.Dictionary.Read, Json.Read],
        DictionaryComponent.Write[Json.Dictionary.Write, Json.Write]

  object enumeration
      extends EnumerationComponent[Json.Enumeration, Json.Primitive],
        EnumerationComponent.Write[Json.Enumeration.Write, Json.Primitive.Write]

  object branch
      extends BranchComponent[Json.Branch, Json],
        BranchComponent.Read[Json.Branch.Read, Json.Read],
        BranchComponent.Write[Json.Branch.Write, Json.Write]

  object field
      extends FieldComponent[Json.Field, Json],
        FieldComponent.Read[Json.Field.Read, Json.Read],
        FieldComponent.Write[Json.Field.Write, Json.Write]

object JsonComponent extends JsonComponent
