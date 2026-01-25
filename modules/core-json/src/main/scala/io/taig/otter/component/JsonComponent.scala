package io.taig.otter.component

import io.taig.otter.Json

trait JsonComponent
    extends CoerceComponent[Json.Coerce, Json.Primitive],
      CoerceComponent.Read[Json.Coerce.Read, Json.Primitive.Read],
      CoerceComponent.Write[Json.Coerce.Write, Json.Primitive.Write],
      PrimitiveComponent.Boolean[Json.Primitive.Boolean],
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
