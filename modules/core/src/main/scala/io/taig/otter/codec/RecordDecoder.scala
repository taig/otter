package io.taig.otter.codec

import cats.data.Validated
import cats.implicits.*
import cats.kernel.Order
import io.taig.otter.Record
import io.taig.otter.Violations
import io.taig.otter.schema.FieldSchema

import scala.collection.immutable.SortedSet

final class RecordDecoder[S[_], Key[_], Value[_], T: Order, U](
    field: Decoder.Remainding[S, List[(T, U)]],
    key: Encoder[Key, T]
)(using FieldSchema[S, Key, Value]):
  def decode[A](schema: Record[S, A], values: List[(T, U)]): Validated[Violations, (List[(T, U)], A)] =
    schema match
      case Record.Empty(_)           => (values, ()).valid
      case Record.Root(field, _)     => this.field.decodeRemainding(schema = field.value, values)
      case Record.Modify(self, f, _) => decode(schema = self, values).map(_.map(f))
      case Record.Optional(self) =>
        val keys = self.fields.map(_.value.key).map(ReferenceConstantEncoder(encoder = key)(_))
        val references = SortedSet.from(values.map((key, _) => key))
        if keys.forall(!references.contains_(_))
        then (values, none).valid
        else decode(schema = self, values).map(_.map(_.some))
      case Record.Zip(left, right, _) =>
        decode(schema = left, values) match
          case Validated.Valid((values, a)) =>
            decode(schema = right, values) match
              case Validated.Valid((values, b))      => (values, (a, b)).valid
              case violations @ Validated.Invalid(_) => violations
          case Validated.Invalid(left) =>
            decode(schema = right, values) match
              case Validated.Valid((_, _))           => left.invalid
              case violations @ Validated.Invalid(_) => violations
