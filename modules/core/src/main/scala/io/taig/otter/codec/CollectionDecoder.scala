package io.taig.otter.codec

import scala.collection.View.Collect
import io.taig.otter.Collection
import cats.syntax.all.*
import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.Constraint
import scala.util.chaining.*

final class CollectionDecoder[-S[_], T](decoder: Decoder[S, T]) extends Decoder[Collection[S, *], Seq[T]]:
  override def decode[A](schema: Collection[S, A], values: Seq[T]): Validated[Violations, A] = schema match
    case Collection.Indexed(schema, validation) =>
      val vector = values.toVector

      validation
        .validate(vector)
        .toValidated
        .leftMap(Violations.apply)
        .andThen: _ =>
          vector.zipWithIndex
            .traverse((value, index) => decoder.decode(schema = schema.value, value).leftMap(index /: _))
    case Collection.Linked(schema, validation) =>
      val list = values.toList

      validation
        .validate(list)
        .toValidated
        .leftMap(Violations.apply)
        .andThen: _ =>
          list.zipWithIndex
            .traverse((value, index) => decoder.decode(schema = schema.value, value).leftMap(index /: _))
    case Collection.Modify(self, f, _) => decode(schema = self, values).map(f)

object CollectionDecoder:
  def apply[S[_], T](decoder: Decoder[S, T]): Decoder[Collection[S, *], Seq[T]] =
    new CollectionDecoder(decoder)
