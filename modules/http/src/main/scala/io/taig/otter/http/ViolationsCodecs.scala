package io.taig.otter.http

import io.taig.otter.Dsl.*
import cats.syntax.all.*
import cats.data.NonEmptyList

object ViolationsCodecs:
  object violations:
    val structured: Sum.Nested.Required[Violations] =
      def comparison[A](reference: Codec.Required[A]): Record.Required[Comparison[A]] =
        record(field("reference", reference) :* field("exclusive", boolean)).to

      val constraint: Sum.Nested.Required[Constraint] = sum.nested {
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

      val step: Sum.Untagged.Required.Of[Data.Primitive, Option[Step]] = sum
        .untagged {
          branch("root", string(matches = ".")) |
            branch("index", int.to[Step.Index]) |
            branch("field", string.to[Step.Field])
        }
        .imap {
          case _: String  => none
          case step: Step => step.some
        } {
          case Some(step: (Step.Index | Step.Field)) => step
          case None                                  => "."
        }

      val root: Codec.Required[Violations.Root] = collection.nonEmptyChain(violation).to
      val namespace: Codec.Required[Violations.Namespace] = dictionary.nonEmptyMap(step, structured).to
      sum.nested(branch("root", root) :+ branch("namespace", namespace)).to

    val listed: Collection.Required.Of[Data.Primitive, List[String]] =
      // parser(name = "violations") { value =>
      //   NonEmptyList.fromList(value.split("\n").toList).flatMap(Violations.parse)
      // }(_.print.mkString_("\n"))
      // collection.list()
      ???

    val flattened: Primitive.Required[Violations] = parser(name = "violations") { value =>
      NonEmptyList.fromList(value.split("\n").toList).flatMap(Violations.parse(_).toOption)
    }(_.print.mkString_("\n"))
