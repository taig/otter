package io.taig.otter.component
import io.taig.otter.Constraint
import io.taig.otter.schema.RecordSchema
import io.taig.otter.schema.FieldSchema
import io.taig.otter.schema.UnionSchema

trait ConstraintComponent[
    Collection[a] <: Value[a],
    Constant[a] <: Value[a],
    Dictionary[a] <: Value[a],
    Nullable[a] <: Value[a],
    Primitive[a] <: Value[a],
    Record[a] <: Value[a],
    Union[a] <: Value[a],
    Field[_],
    Key[_],
    Value[_]
](using FieldSchema[Field, Key, Value, Record], RecordSchema[Record, Field], UnionSchema[Union, Value])
    extends ComparisonComponent[Nullable, Record, Field, Key, Value],
      DataComponent[Collection, Constant, Dictionary, Nullable, Primitive, Record, Union, Field, Key, Value],
      FieldComponent.Primitive.String[Field, Key, Value, Record],
      PrimitiveComponent[Primitive],
      NullableComponent[Nullable, Value],
      RecordComponent[Record, Field],
      SumComponent[Constant, Record, Field, Key, Value]:
  this: PrimitiveComponent.String[Value] =>

  val constraint: Union[Constraint] =
    val col: Union[Constraint.Collection] = (
      merged("collection.maximum", field("reference", int).toRecord).to[Constraint.Collection.Maximum] :+
        merged("collection.minimum", field("reference", int).toRecord).to[Constraint.Collection.Minimum] :+
        merged("collection.unique").as(Constraint.Collection.Unique)
    ).to

    val obj: Union[Constraint.Object] = (
      merged("object.maximum", field("reference", int).toRecord).to[Constraint.Object.Maximum] :+
        merged("object.minimum", field("reference", int).toRecord).to[Constraint.Object.Minimum]
    ).to

    val primitive: Union[Constraint.Primitive] =
      val number: Union[Constraint.Primitive.Number] = (
        merged("primitive.number.maximum", field("comparison", comparison(data.number)).toRecord)
          .to[Constraint.Primitive.Number.Maximum] :+
          merged("primitive.number.minimum", field("comparison", comparison(data.number)).toRecord)
            .to[Constraint.Primitive.Number.Minimum] :+
          merged("primitive.number.multiple", field("reference", data.number).toRecord)
            .to[Constraint.Primitive.Number.Multiple]
      ).to

      val string: Union[Constraint.Primitive.String] = (
        merged("primitive.string.matches", field("pattern", pattern).toRecord)
          .to[Constraint.Primitive.String.Matches] :+
          merged("primitive.string.maximum", field("reference", int).toRecord)
            .to[Constraint.Primitive.String.Maximum] :+
          merged("primitive.string.minimum", field("reference", int).toRecord).to[Constraint.Primitive.String.Minimum]
      ).to

      number.orElse(string).to

    (
      merged("equal", field("reference", data.any).toRecord).to[Constraint.Equal] :+
        merged("oneOf", field("values", collection.list(data.any)).toRecord).to[Constraint.OneOf] :+
        merged("required").as(Constraint.Required) :+
        merged("type", field("name", string).toRecord).to[Constraint.Type]
    ).orElse(col).orElse(obj).orElse(primitive).to
