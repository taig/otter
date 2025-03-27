package io.taig.otter

import io.circe.Json
import cats.syntax.all.*
import cats.data.Validated

object CirceJsonConstantDecoder:
  def apply[A](codec: Constant[?, A], json: Json): Validated[Violations, A] = codec match
    case Constant.Modify(self, f, _) => CirceJsonConstantDecoder(codec = self, json).map(f)
    case self @ Constant.Root(codec, reference, _) =>
      CirceJsonCodecDecoder(codec = codec.value, json).andThen: a =>
        Validated.cond(
          test = self.matches(a),
          a,
          Violations.rootNec(
            Violation.tpe(name = StringCodecPrinter(codec = codec.value, reference), actual = toValue(json))
          )
        )
