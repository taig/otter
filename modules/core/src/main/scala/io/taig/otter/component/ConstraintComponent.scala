package io.taig.otter.component

import cats.Invariant
import io.taig.otter.Constraint
import io.taig.otter.syntax.InvariantSyntax.*

trait ConstraintComponent[
    Collection[a] <: Value[a],
    Dictionary[a] <: Value[a],
    Nullable[a] <: Value[a],
    Primitive[a] <: Value[a],
    Record[a] <: Value[a],
    Sum[a] <: Value[a]: Invariant,
    Branch[_]: Invariant,
    Field[_],
    Key[_],
    Value[_]
] extends BranchComponent.Primitive.String[Branch, Key, Value, Sum],
      ComparisonComponent[Nullable, Record, Field, Key, Value],
      DataComponent[Collection, Dictionary, Nullable, Primitive, Sum, Branch, Key, Value],
      FieldComponent.Primitive.String[Field, Key, Value, Record],
      PrimitiveComponent[Primitive],
      NullableComponent[Nullable, Value],
      SumComponent[Sum, Branch]:
  val constraint: Sum[Constraint] =
    val col: Sum[Constraint.Collection] = (
      branch("collection.maximum", field("reference", int).toRecord).to[Constraint.Collection.Maximum] :+
        branch("collection.minimum", field("reference", int).toRecord).to[Constraint.Collection.Minimum] :+
        branch("collection.unique", void).as(Constraint.Collection.Unique)
    ).to

    val obj: Sum[Constraint.Object] = (
      branch("object.maximum", field("reference", int).toRecord).to[Constraint.Object.Maximum] :+
        branch("object.minimum", field("reference", int).toRecord).to[Constraint.Object.Minimum]
    ).to

    val primitive: Sum[Constraint.Primitive] =
      val number: Sum[Constraint.Primitive.Number] = (
        branch("primitive.number.maximum", field("comparison", comparison(data.number)).toRecord)
          .to[Constraint.Primitive.Number.Maximum] :+
          branch("primitive.number.minimum", field("comparison", comparison(data.number)).toRecord)
            .to[Constraint.Primitive.Number.Minimum] :+
          branch("primitive.number.multiple", field("reference", data.number).toRecord)
            .to[Constraint.Primitive.Number.Multiple]
      ).to

      val string: Sum[Constraint.Primitive.String] = (
        branch("primitive.string.matches", field("pattern", pattern).toRecord)
          .to[Constraint.Primitive.String.Matches] :+
          branch("primitive.string.maximum", field("reference", int).toRecord)
            .to[Constraint.Primitive.String.Maximum] :+
          branch("primitive.string.minimum", field("reference", int).toRecord).to[Constraint.Primitive.String.Minimum]
      ).to

      number.orElse(string).to

    (
      branch("equal", field("reference", data.any).toRecord).to[Constraint.Equal] :+
        branch("oneOf", field("values", collection.list(data.any)).toRecord).to[Constraint.OneOf] :+
        branch("required", void).as(Constraint.Required) :+
        branch("type", field("name", string).toRecord).to[Constraint.Type]
    ).orElse(col).orElse(obj).orElse(primitive).to
