package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Constraint
import io.taig.otter.Dictionary
import io.taig.otter.Violations
import io.taig.validation.Validation

import scala.collection.immutable.SortedMap

final class DictionaryDecoder[K[-_, +_], F[-_, +_], T](key: Decoder[K, String], decoder: Decoder[F, T])
    extends Decoder[[w, r] =>> Dictionary[K, F, w, r], List[(String, T)]]:
  override def decode[R](
      schema: Dictionary[K, F, Nothing, R],
      values: List[(String, T)]
  ): Validated[Violations, R] = schema match
    case Dictionary.Hashed(keys, reference, ordering, validation) =>
      entries(keys.value, reference.value, values, validation)(SortedMap.from(_)(using ordering))
    case Dictionary.Linked(keys, reference, validation) =>
      entries(keys.value, reference.value, values, validation)(identity)
    case Dictionary.Modify(self, f, _) => decode(self, values).map(f)

  /** A key that failed to parse has no read side to report under, so a violation stays at the text the document
    * actually holds. Both halves of an entry are read before either is given up on, so a bad key and a bad value in the
    * same entry are reported together.
    */
  private def entries[KR, R, C](
      key: K[Nothing, KR],
      schema: F[Nothing, R],
      values: List[(String, T)],
      validation: Validation[Constraint.Object, C]
  )(collect: List[(KR, R)] => C): Validated[Violations, C] =
    values
      .traverse: (raw, value) =>
        (this.key.decode(key, raw), decoder.decode(schema, value)).tupled.leftMap(raw /: _)
      .map(collect)
      .andThen(values => validation.validate(values).toInvalid(values).leftMap(Violations.apply))
