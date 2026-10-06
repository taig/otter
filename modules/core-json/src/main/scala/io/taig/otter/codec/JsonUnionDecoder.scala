package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.data.Data
import io.taig.data.syntax.*
import io.taig.otter.Constraint
import io.taig.otter.Json
import io.taig.otter.Violations
import io.taig.otter.component.JsonComponent
import io.taig.validation.Violation

/** The document model supplies object access; tag validation and dispatch are shared by all JSON interpreters. */
final class JsonUnionDecoder[T](
    decoder: Decoder[Json.Node, T],
    branch: Decoder[Json.Branch.Node, T],
    members: T => Validated[Violations, Iterable[(String, T)]]
) extends Decoder[Json.Union.Node, T]:
  private val untagged = UnionDecoder(branch)

  override def decode[R](schema: Json.Union.Node[Nothing, R], value: T): Validated[Violations, R] =
    schema.discriminator match
      case None      => untagged.decode(schema.self.self, value)
      case Some(key) =>
        members(value).andThen: fields =>
          fields
            .find(_._1 == key)
            .map(_._2)
            .toValid(Violations(Violation(Constraint.Generic.Required, Data.Null, None)))
            .andThen(decoder.decode(JsonComponent.string, _))
            .andThen: tag =>
              schema.readers
                .get(tag)
                .toValid:
                  val allowed = schema.self.self.branches.toNonEmptyList.toList.map(_.value.self.self.name.asData)
                  Violations(Violation(Constraint.Generic.OneOf(allowed), tag.asData, None))
            .leftMap(key /: _)
            .andThen(decoder.decode(_, value))
