package io.taig.otter.component

import io.taig.otter.operation.*
import io.taig.otter.syntax.EnrichedSyntax.*
import io.taig.otter.validation.Constraint

trait ConstraintComponent[
    Collection[a] <: Value[a],
    Constant[a] <: Value[a],
    Dictionary[a] <: Value[a],
    Primitive[a] <: Value[a],
    Record[a] <: Value[a],
    Union[a] <: Value[a],
    Field[_],
    Key[_],
    Value[_]
](using
    SchemaInvariant[Record],
    UnionSchemaInvariant[Union, Value],
    SchemaInvariant.Recordable[Field, Record],
    SchemaInvariant.Unionable[Value, Union]
) extends ComparisonComponent[Record, Field, Key, Value],
      DataComponent[Collection, Dictionary, Primitive, Union, Key, Value],
      SumComponent[Constant, Primitive, Record, Field, Key, Value]:
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

      val string: Union[Constraint.Primitive.Text] = (
        merged("primitive.string.matches", field("pattern", pattern).toRecord)
          .to[Constraint.Primitive.Text.Matches] :+
          merged("primitive.string.maximum", field("reference", int).toRecord)
            .to[Constraint.Primitive.Text.Maximum] :+
          merged("primitive.string.minimum", field("reference", int).toRecord).to[Constraint.Primitive.Text.Minimum]
      ).to

      number.orElse(string).to

    (
      merged("equal", field("reference", data.any).toRecord).to[Constraint.Equal] :+
        merged("oneOf", field("values", collection.list(data.any)).toRecord).to[Constraint.OneOf] :+
        merged("required").as(Constraint.Required) :+
        merged("type", field("name", string).toRecord).to[Constraint.Type]
    ).orElse(col)
      .orElse(obj)
      .orElse(primitive)
      .name("Constraint")
      .to
