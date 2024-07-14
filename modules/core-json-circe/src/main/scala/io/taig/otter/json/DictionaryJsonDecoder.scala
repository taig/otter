package io.taig.otter.json

import cats.syntax.all.*
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import io.circe.Json
import io.circe.syntax.*
import io.taig.otter.*

object DictionaryJsonDecoder:
  def apply[A](schema: Dictionary.Via[Json, A], values: Option[List[(String, Json)]]): Decoder.Result[Json, A] =
    schema match
      case Dictionary.Optional(self) => values.fold(none.valid)(_ => DictionaryJsonDecoder(self, values).map(_.some))
      case Dictionary.Root(_, key, value) =>
        values
          .toValid(Violations.rootNec(Violation(Constraint.Type(name = "array"), actual = "null".asJson)))
          .andThen: values =>
            values.traverse { case (a, b) =>
              (
                ValueRequiredStringDecoder(key, a).leftMap(_.bimap(_.map(_.asJson), _.asJson)),
                JsonDecoder(value, b)
              ).tupled.leftMap(a /: _)
            }
      case Dictionary.Transform(self, f, _) => DictionaryJsonDecoder(self, values).map(f)
