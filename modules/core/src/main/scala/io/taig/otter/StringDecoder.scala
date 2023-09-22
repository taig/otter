package io.taig.otter

import cats.syntax.all.*
import cats.data.{Chain, Validated}
import io.taig.otter.Schema.Collection
import io.taig.otter.validation.{Violation, Violations}

object StringDecoder:
  val value: Decoder[Schema.Value, Option[String]] = new Decoder:
    override def decode[B](schema: Schema.Value[B], a: Option[String]): Validated[Violations, B] = ???

  val collection: Decoder[Schema.Collection[Schema.Value, *], Option[Chain[String]]] = new Decoder:
    override def decode[B](
        schema: Schema.Collection[Schema.Value, B],
        a: Option[Chain[String]]
    ): Validated[Violations, B] = schema match
      case Collection.Root(schema, _, _) =>
        Validated
          .fromOption(a, Violations.rootNec(Violation.required))
          .andThen(_.zipWithIndex.traverse { case (value, index) =>
            StringDecoder.value.decode(schema, value.some).leftMap(_.modifyHistory(index /: _))
          })
      case Collection.Optional(self)                => a.traverse(values => decode(self, values.some))
      case Collection.Validate(self, validation, _) => decode(self, a).andThen(validation(_).leftMap(Violations.root))
