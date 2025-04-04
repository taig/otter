// package io.taig.otter.http

// import cats.syntax.all.*
// import io.taig.otter.Dsl.*

// object ViolationsCodecs:
//   object violations:
//     val structured: Union.Of[Data.Object[?], Violations] =
//       def comparison[A](reference: Codec[A]): Record[Comparison[A]] =
//         (field("reference", reference) :* field("exclusive", boolean)).to

//       val constraint: Union[Constraint] = (
//         branch.keyed("type", string).to[Constraint.Type] :+
//           branch.keyed("oneOf", collection.list(dynamic.primitive)).to[Constraint.OneOf]
//       ).orElse {
//         (
//           branch.keyed("maxItems", int).to[Constraint.Collection.MaxItems] :+
//             branch.keyed("maxItems", int).to[Constraint.Collection.MinItems] :+
//             branch.keyed("uniqueItems", singleton(Constraint.Collection.UniqueItems))
//         ).to[Constraint.Collection]
//       }.orElse {
//         (
//           branch.keyed("maxProperties", int).to[Constraint.Object.MaxProperties] :+
//             branch.keyed("minProperties", int).to[Constraint.Object.MinProperties]
//         ).to[Constraint.Object]
//       }.orElse {
//         (
//           branch.keyed("matches", pattern).to[Constraint.Primitive.Matches] :+
//             branch.keyed("maximum", comparison(dynamic.number)).to[Constraint.Primitive.Maximum] :+
//             branch.keyed("minimum", comparison(dynamic.number)).to[Constraint.Primitive.Minimum] :+
//             branch.keyed("maxLength", int).to[Constraint.Primitive.MaxLength] :+
//             branch.keyed("minLength", int).to[Constraint.Primitive.MinLength] :+
//             branch.keyed("multiple", dynamic.number).to[Constraint.Primitive.Multiple]
//         ).to[Constraint.Primitive]
//       }.to

//       val violation: Record[Violation] = (field("constraint", constraint) :* field("actual", dynamic.any)).to

//       val step: Primitive[Step] = parser(name = "step")(Step.parse(_).toOption)(_.show)

//       val root: Record[Violations.Root] = (
//         field("values", dictionary.sortedMap(step, structured)) :*
//           field("violations", collection.nonEmptyChain(violation))
//       ).to

//       val namespace: Dictionary[Violations.Namespace] = dictionary.nonEmptyMap(step, structured).to

//       (branch("root", root) :+ branch("namespace", namespace)).to

//     val text: Primitive[Violations] =
//       parser(name = "violations")(Violations.parse(_).toOption)(_.show)
