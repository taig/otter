// package io.taig.otter

// trait ConstraintDsl[
//     Collection[a] <: Value[a],
//     Dictionary[a] <: Value[a],
//     Nullable[a] <: Value[a],
//     Primitive[a] <: Value[a],
//     Record[a] <: Value[a],
//     Sum[a] <: Value[a],
//     Union[a] <: Value[a],
//     Branch[_],
//     Field[_],
//     Key[_],
//     Value[_]
// ](using Codec.Sum[Sum, Branch], Codec.Branch[Branch, Key, Value, Sum], Codec.Field[Field, Key, Value, Record])
//     extends BranchDsl.Primitive.String[Branch, Key, Value, Sum],
//       ComparisonDsl[Record, Field, Key, Value],
//       CollectionDsl[Collection, Value],
//       DataDsl[Collection, Dictionary, Nullable, Primitive, Sum, Union, Branch, Key, Value],
//       FieldDsl.Primitive.String[Field, Key, Value, Record],
//       NullableDsl[Nullable, Value],
//       PrimitiveDsl[Primitive]:

//   val constraint: Sum[Constraint] =
//     val col: Sum[Constraint.Collection] = (
//       branch("maxItems", field("reference", int).toRecord).to[Constraint.Collection.MaxItems] :+
//         branch("minItems", field("reference", int).toRecord).to[Constraint.Collection.MinItems] :+
//         branch("uniqueItems", void).as(Constraint.Collection.UniqueItems)
//     ).to

//     val obj: Sum[Constraint.Object] = (
//       branch("maxProperties", field("reference", int).toRecord).to[Constraint.Object.MaxProperties] :+
//         branch("minProperties", field("reference", int).toRecord).to[Constraint.Object.MinProperties]
//     ).to

//     val primitive: Sum[Constraint.Primitive] = (
//       branch("matches", field("pattern", pattern).toRecord).to[Constraint.Primitive.Matches] :+
//         branch("maximum", field("comparison", comparison(data.number)).toRecord).to[Constraint.Primitive.Maximum] :+
//         branch("minimum", field("comparison", comparison(data.number)).toRecord).to[Constraint.Primitive.Minimum] :+
//         branch("maxLength", field("reference", int).toRecord).to[Constraint.Primitive.MaxLength] :+
//         branch("minLength", field("reference", int).toRecord).to[Constraint.Primitive.MinLength] :+
//         branch("multiple", field("reference", data.number).toRecord).to[Constraint.Primitive.Multiple]
//     ).to

//     (
//       branch("equal", field("reference", data.any).toRecord).to[Constraint.Equal] :+
//         branch("oneOf", field("values", collection.list(data.primitive)).toRecord).to[Constraint.OneOf] :+
//         branch("required", void).as(Constraint.Required) :+
//         branch("type", field("name", string).toRecord).to[Constraint.Type]
//     ).orElse(col).orElse(obj).orElse(primitive).to
