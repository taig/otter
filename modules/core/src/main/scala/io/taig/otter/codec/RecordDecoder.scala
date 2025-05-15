package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Record
import io.taig.otter.codec.Decoder
import io.taig.otter.collectFirstWithRemainders
import io.taig.otter.Violations
import io.taig.otter.Violation

final class RecordDecoder[S[_], T[_], U](key: Codec[S, String], value: Decoder[T, U]):
  def apply[A](
      codec: Record[S, T, A],
      values: List[(String, U)]
  ): Validated[Violations, (List[(String, U)], A)] = codec match
    case Record.Empty(_)           => (values, ()).valid
    case Record.Root(field, _)     => FieldDecoder(key, value)(field, values)
    case Record.Modify(self, f, _) => apply(codec = self, values).map(_.map(f))
    case Record.Optional(self) =>
      val keys = self.fields.map((field) => ReferenceConstantEncoder(key)(field.key))
      val references = values.map((key, _) => key).toSet

      if keys.forall(!references.contains(_))
      then (values, none).valid
      else apply(codec = self, values).map(_.map(_.some))
    case Record.Zip(left, right, _) =>
      apply(codec = left, values) match
        case Validated.Valid((values, a)) =>
          apply(codec = right, values) match
            case Validated.Valid((values, b))      => (values, (a, b)).valid
            case violations @ Validated.Invalid(_) => violations
        case Validated.Invalid(left) =>
          apply(codec = right, values) match
            case Validated.Valid((_, _))           => left.invalid
            case violations @ Validated.Invalid(_) => violations
