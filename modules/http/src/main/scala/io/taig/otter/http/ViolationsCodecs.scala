package io.taig.otter.http

import io.taig.otter.Dsl.*
import cats.syntax.all.*
import cats.data.NonEmptyList
import cats.data.NonEmptyChain

object ViolationsCodecs:
  object violations:
    val nested: Sum.Nested.Required[Violations] =
      def comparison[A](reference: Codec.Required[A]): Record.Required[Comparison[A]] =
        record(field("reference", reference) :* field("exclusive", boolean)).to

      val constraint: Sum.Keyed.Required[Constraint] = sum.keyed {
        (
          branch("type", record(field("name", string)).to[Constraint.Type]) :+
            branch("oneOf", record(field("values", collection.nonEmptyList(dynamic.primitive))).to[Constraint.OneOf])
        ).orElse {
          (
            branch("maxItems", record(field("reference", int)).to[Constraint.Collection.MaxItems]) :+
              branch("maxItems", record(field("reference", int)).to[Constraint.Collection.MinItems]) :+
              branch("uniqueItems", singleton(Constraint.Collection.UniqueItems))
          ).to[Constraint.Collection]
        }.orElse {
          (
            branch("maxProperties", record(field("reference", int)).to[Constraint.Object.MaxProperties]) :+
              branch("minProperties", record(field("reference", int)).to[Constraint.Object.MinProperties])
          ).to[Constraint.Object]
        }.orElse {
          (
            branch("matches", record(field("pattern", pattern)).to[Constraint.Primitive.Matches]) :+
              branch("maximum", comparison(dynamic.number).to[Constraint.Primitive.Maximum]) :+
              branch("minimum", comparison(dynamic.number).to[Constraint.Primitive.Minimum]) :+
              branch("maxLength", record(field("reference", int)).to[Constraint.Primitive.MaxLength]) :+
              branch("minLength", record(field("reference", int)).to[Constraint.Primitive.MinLength]) :+
              branch("multiple", record(field("reference", dynamic.number)).to[Constraint.Primitive.Multiple])
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

      sum.nested(branch("root", root) :+ branch("namespace", namespace)).to

    def flattened: Dictionary.Required.Of[Data.Primitive, Violations] =
      val violation = parser[Violation](name = "violation")(Violation.parse(_).toOption)(_.show)

      dictionary
        .nonEmptyList(xpath, violation)
        .imap(_.groupMapNem { case (xpath, _) => xpath } { case (_, violation) => violation }) {
          _.toNel.flatMap { case (xpath, violations) => violations.tupleLeft(xpath) }
        }
        .imap(_.map(NonEmptyChain.fromNonEmptyList))(_.map(_.toNonEmptyList))
        .imap(Violations.from)(_.toNem)

    val printed: Primitive.Required[Violations] =
      parser(name = "violations")(Violations.parse(_).toOption)(_.show)
