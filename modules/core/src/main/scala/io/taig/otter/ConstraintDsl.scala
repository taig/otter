package io.taig.otter

trait ConstraintDsl[
    Collection[a] <: Value[a],
    Dictionary[a] <: Value[a],
    Nullable[a] <: Value[a],
    Primitive[a] <: Value[a],
    Record[a] <: Value[a],
    Union[a] <: Value[a],
    Field[_],
    Key[_],
    Value[_]
](using Codec.Union[Union, Value], Codec.Field[Field, Key, Value, Record])
    extends ComparisonDsl[Record, Field, Key, Value],
      CollectionDsl[Collection, Value],
      DataDsl[Collection, Dictionary, Nullable, Primitive, Union, Key, Value],
      NullableDsl[Nullable, Value],
      UnionDsl[Union, Value],
      PrimitiveDsl[Primitive],
      FieldDsl.Primitive.String[Field, Key, Value, Record]:

  val constraint: Union[Constraint] =
    val col: Union[Constraint.Collection] = (
      branch("maxItems", field("reference", int).toRecord).to[Constraint.Collection.MaxItems] :+
        branch("minItems", field("reference", int).toRecord).to[Constraint.Collection.MinItems] :+
        branch("uniqueItems", void).as(Constraint.Collection.UniqueItems)
    ).to

    val obj: Union[Constraint.Object] = (
      branch("maxProperties", field("reference", int).toRecord).to[Constraint.Object.MaxProperties] :+
        branch("minProperties", field("reference", int).toRecord).to[Constraint.Object.MinProperties]
    ).to

    val primitive: Union[Constraint.Primitive] = (
      branch("matches", field("pattern", pattern).toRecord).to[Constraint.Primitive.Matches] :+
        branch("maximum", field("comparison", comparison(data.number)).toRecord).to[Constraint.Primitive.Maximum] :+
        branch("minimum", field("comparison", comparison(data.number)).toRecord).to[Constraint.Primitive.Minimum] :+
        branch("maxLength", field("reference", int).toRecord).to[Constraint.Primitive.MaxLength] :+
        branch("minLength", field("reference", int).toRecord).to[Constraint.Primitive.MinLength] :+
        branch("multiple", field("reference", data.number).toRecord).to[Constraint.Primitive.Multiple]
    ).to

    (
      branch("equal", field("reference", data.any).toRecord).to[Constraint.Equal] :+
        branch("oneOf", field("values", collection.list(data.primitive)).toRecord).to[Constraint.OneOf] :+
        branch("required", void).as(Constraint.Required) :+
        branch("type", field("name", string).toRecord).to[Constraint.Type]
    ).orElse(col).orElse(obj).orElse(primitive).merged.to
