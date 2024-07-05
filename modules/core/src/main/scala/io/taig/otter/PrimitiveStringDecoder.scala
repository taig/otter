package io.taig.otter

import io.taig.otter.validation.Violations
import cats.syntax.all.*

object PrimitiveStringDecoder:
  def apply[A](schema: Primitive.Required.Reader[A], value: String): Decoder.Result[String, A] = schema match
    case Primitive.Required.Reader.Transform(self, validation) =>
      PrimitiveStringDecoder(self, value).andThen: a =>
        val x = validation(a) // .leftMap(Violations.root)
        x.leftMap(_.map(_.bimap(x => x.map(writer => ???), x => ???)))
        ???
    case Primitive.Required.Transform(self, _, f) => ??? // apply(self, f(a))
    case Primitive.Required.Root(tpe)             => ??? // TypeStringEncoder(tpe, a)
