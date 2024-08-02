package io.taig.otter.http

import io.taig.otter.Dsl.*
import cats.syntax.all.*

// Scala.js compilation fails when defining those directly in Codecs, so we put them here
// and reexport from Codecs
private object ViolationsCodecs:
  val violations: Sum.Nested[Violations[Violation[Constraint.Any[Data], Data]]] =
    val constraint: Sum.Nested[Constraint.Any[Data]] = sum
      .nested {
        branch("type", record(field("name", string)).to[Constraint.Type]) :+
          branch("oneOf", record(field("values", collection.list(dynamic.any))).to[Constraint.OneOf[Data]])
      }
      .to[Constraint[Data]] | sum
      .nested {
        branch("maxItems", record(field("reference", long)).to[Constraint.Collection.MaxItems]) :+
          branch("maxItems", record(field("reference", long)).to[Constraint.Collection.MinItems]) :+
          branch("uniqueItems", singleton(Constraint.Collection.UniqueItems))
      }
      .to[Constraint.Collection] | sum
      .nested {
        branch("maxProperties", record(field("reference", long)).to[Constraint.Object.MaxProperties]) :+
          branch("minProperties", record(field("reference", long)).to[Constraint.Object.MinProperties])
      }
      .to[Constraint.Object] | sum
      .nested {
        branch("matches", record(field("pattern", pattern)).to[Constraint.Primitive.Matches]) :+
          branch(
            "maximum",
            record(field("reference", dynamic.any) :* field("exclusive", boolean))
              .to[Constraint.Primitive.Maximum[Data]]
          ) :+
          branch(
            "minimum",
            record(field("reference", dynamic.any) :* field("exclusive", boolean))
              .to[Constraint.Primitive.Minimum[Data]]
          ) :+
          branch("maxLength", record(field("reference", int)).to[Constraint.Primitive.MaxLength]) :+
          branch("minLength", record(field("reference", int)).to[Constraint.Primitive.MinLength]) :+
          branch("multiple", record(field("reference", dynamic.any)).to[Constraint.Primitive.Multiple[Data]])
      }
      .to[Constraint.Primitive[Data]]

    val violation: Record[Violation[Constraint.Any[Data], Data]] =
      record(field("constraint", constraint) :* field("actual", dynamic.any)).to

    val step: Sum.Untagged.Required.Of[Data.Primitive, Option[Step]] =
      sum
        .untagged {
          branch("root", string.ivalidate_(matches("."))) |
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

    val root: Codec.Required[Violations.Root[Violation[Constraint.Any[Data], Data]]] =
      collection.nonEmptyChain(violation).to
    val namespace: Codec.Required[Violations.Namespace[Violation[Constraint.Any[Data], Data]]] =
      dictionary.nonEmptyMap(step, violations).to

    sum.nested(branch("root", root) :+ branch("namespace", namespace)).to
