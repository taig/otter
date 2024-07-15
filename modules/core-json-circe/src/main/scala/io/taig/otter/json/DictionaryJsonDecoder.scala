package io.taig.otter.json

import cats.syntax.all.*
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import io.circe.Json
import io.circe.syntax.*
import io.taig.otter.*

object DictionaryJsonDecoder:
  def apply[A](schema: Dictionary[?, A], values: Option[List[(String, Json)]]): Decoder.Result[Data, A] =
    schema match
      case Dictionary.Optional(self) => values.fold(none.valid)(_ => DictionaryJsonDecoder(self, values).map(_.some))
      case Dictionary.Root(_, key, value) =>
        values
          .toValid(Violations.rootNec(Violation(Constraint.Type(name = "array"), actual = Data.String("null"))))
          .andThen: values =>
            values.traverse { case (a, b) =>
              (ValueRequiredStringDecoder(key, a), JsonDecoder(value, b)).tupled.leftMap(a /: _)
            }
      case Dictionary.Transform(self, f, _) => DictionaryJsonDecoder(self, values).map(f)
