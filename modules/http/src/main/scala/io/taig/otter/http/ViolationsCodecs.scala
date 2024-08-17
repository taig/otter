package io.taig.otter.http

import io.taig.otter.Dsl.*
import cats.syntax.all.*

object ViolationsCodecs:
  object violations:
    val nested: Sum.Untagged.Required[Violations] =
      def comparison[A](reference: Codec.Required[A]): Record.Required[Comparison[A]] =
        record(field("reference", reference) :* field("exclusive", boolean)).to[Comparison[A]]

      val constraint: Sum.Keyed.Required[Constraint] = sum.keyed {
        (
          branch("type", string.to[Constraint.Type]) :+
            branch("oneOf", collection.list(dynamic.primitive).to[Constraint.OneOf])
        ).orElse {
          (
            branch("maxItems", int.to[Constraint.Collection.MaxItems]) :+
              branch("maxItems", int.to[Constraint.Collection.MinItems]) :+
              branch("uniqueItems", singleton(Constraint.Collection.UniqueItems))
          ).to[Constraint.Collection]
        }.orElse {
          (
            branch("maxProperties", int.to[Constraint.Object.MaxProperties]) :+
              branch("minProperties", int.to[Constraint.Object.MinProperties])
          ).to[Constraint.Object]
        }.orElse {
          (
            branch("matches", pattern.to[Constraint.Primitive.Matches]) :+
              branch("maximum", comparison(dynamic.number).to[Constraint.Primitive.Maximum]) :+
              branch("minimum", comparison(dynamic.number).to[Constraint.Primitive.Minimum]) :+
              branch("maxLength", int.to[Constraint.Primitive.MaxLength]) :+
              branch("minLength", int.to[Constraint.Primitive.MinLength]) :+
              branch("multiple", dynamic.number.to[Constraint.Primitive.Multiple])
          ).to[Constraint.Primitive]
        }
      }.to

      val violation: Record.Required[Violation] =
        record(field("constraint", constraint) :* field("actual", dynamic.any)).to

      val step: Primitive.Required[Step] = parser(name = "step")(Step.parse(_).toOption)(_.show)

      val root: Codec.Required[Violations.Root] = record(
        field("values", dictionary.sortedMap(step, nested)) :*
          field("violations", collection.nonEmptyChain(violation))
      ).to

      val namespace: Codec.Required[Violations.Namespace] = dictionary.nonEmptyMap(step, nested).to

      sum.untagged(branch("root", root) :+ branch("namespace", namespace)).to

    val printed: Primitive.Required[Violations] =
      parser(name = "violations")(Violations.parse(_).toOption)(_.show)
