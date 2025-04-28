package io.taig.otter

trait ConstraintDsl[
    Collection[a] <: Value[a],
    Dictionary[a] <: Value[a],
    Nullable[a] <: Value[a],
    Primitive[a] <: Value[a],
    Record[a] <: Value[a],
    Union[a] <: Value[a],
    Key[_],
    Value[_]
](using Codec.Union[Union, Value])
    extends ComparisonDsl[Record, Key, Value],
      DataDsl[Collection, Dictionary, Nullable, Primitive, Union, Key, Value],
      RecordDsl.Primitive.String[Record, Key, Value]:
  val constraint: Union[Constraint] =
    val col: Union[Constraint.Collection] = (
      branch("maxItems", field("reference", int)).to[Constraint.Collection.MaxItems] :+
        branch("minItems", field("reference", int)).to[Constraint.Collection.MinItems] :+
        branch("uniqueItems", void).as(Constraint.Collection.UniqueItems)
    ).to

    val obj: Union[Constraint.Object] = (
      branch("maxProperties", field("reference", int)).to[Constraint.Object.MaxProperties] :+
        branch("minProperties", field("reference", int)).to[Constraint.Object.MinProperties]
    ).to

    val primitive: Union[Constraint.Primitive] = (
      branch("matches", field("pattern", pattern)).to[Constraint.Primitive.Matches] :+
        branch("maximum", field("comparison", comparison(data.number))).to[Constraint.Primitive.Maximum] :+
        branch("minimum", field("comparison", comparison(data.number))).to[Constraint.Primitive.Minimum] :+
        branch("maxLength", field("reference", int)).to[Constraint.Primitive.MaxLength] :+
        branch("minLength", field("reference", int)).to[Constraint.Primitive.MinLength] :+
        branch("multiple", field("reference", data.number)).to[Constraint.Primitive.Multiple]
    ).to

    (
      branch("equal", field("reference", data.any)).to[Constraint.Equal] :+
        branch("oneOf", field("values", collection.list(data.primitive))).to[Constraint.OneOf] :+
        branch("required", void).as(Constraint.Required) :+
        branch("type", field("name", string)).to[Constraint.Type]
    ).orElse(col).orElse(obj).orElse(primitive).merged.to
