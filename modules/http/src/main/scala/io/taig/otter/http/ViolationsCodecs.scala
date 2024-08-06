package io.taig.otter.http

import io.taig.otter.Dsl.*
import cats.syntax.all.*

object ViolationsCodecs:
  def comparison[A](reference: Codec.Required[A]): Record.Required[Comparison[A]] =
    record(field("reference", reference) :* field("exclusive", boolean)).to

  val violations: Sum.Nested.Required[Violations] =
    val constraint: Sum.Nested.Required[Constraint] = sum
      .nested {
        branch("type", record(field("name", string)).to[Constraint.Type]) :+
          branch("oneOf", record(field("values", collection.nonEmptyList(dynamic.primitive))).to[Constraint.OneOf])
      }
      .to[Constraint] | sum
      .nested {
        branch("maxItems", record(field("reference", int)).to[Constraint.Collection.MaxItems]) :+
          branch("maxItems", record(field("reference", int)).to[Constraint.Collection.MinItems]) :+
          branch("uniqueItems", singleton(Constraint.Collection.UniqueItems))
      }
      .to[Constraint.Collection] | sum
      .nested {
        branch("maxProperties", record(field("reference", int)).to[Constraint.Object.MaxProperties]) :+
          branch("minProperties", record(field("reference", int)).to[Constraint.Object.MinProperties])
      }
      .to[Constraint.Object] | sum
      .nested {
        branch("matches", record(field("pattern", pattern)).to[Constraint.Primitive.Matches]) :+
          branch("maximum", comparison(dynamic.number).to[Constraint.Primitive.Maximum]) :+
          branch("minimum", comparison(dynamic.number).to[Constraint.Primitive.Minimum]) :+
          branch("maxLength", record(field("reference", int)).to[Constraint.Primitive.MaxLength]) :+
          branch("minLength", record(field("reference", int)).to[Constraint.Primitive.MinLength]) :+
          branch("multiple", record(field("reference", dynamic.number)).to[Constraint.Primitive.Multiple])
      }
      .to[Constraint.Primitive]

    val violation: Record.Required[Violation] =
      record(field("constraint", constraint) :* field("actual", dynamic.any)).to

    val step: Sum.Untagged.Required.Of[Data.Primitive, Option[Step]] =
      sum
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
    val namespace: Codec.Required[Violations.Namespace] = dictionary.nonEmptyMap(step, violations).to
    sum.nested(branch("root", root) :+ branch("namespace", namespace)).to
