// package io.taig.otter.codec

// import cats.data.Validated
// import cats.syntax.all.*
// import io.taig.otter.Collection
// import io.taig.otter.Violations

// final class CollectionDecoder[-S[_], T](decoder: Decoder[S, T]) extends Decoder[Collection[S, *], Seq[T]]:
//   override def decode[A](schema: Collection[S, A], values: Seq[T]): Validated[Violations, A] = schema match
//     case Collection.Indexed(schema, validation) =>
//       values.zipWithIndex
//         .traverse((value, index) => decoder.decode(schema = schema.value, value).leftMap(index /: _))
//         .map(_.toVector)
//         .andThen(values => validation.validate(values).toInvalid(values).leftMap(Violations.apply))
//     case Collection.Linked(schema, validation) =>
//       values.zipWithIndex
//         .traverse((value, index) => decoder.decode(schema = schema.value, value).leftMap(index /: _))
//         .map(_.toList)
//         .andThen(values => validation.validate(values).toInvalid(values).leftMap(Violations.apply))
//     case Collection.Modify(self, f, _) => decode(schema = self, values).map(f)

// object CollectionDecoder:
//   def apply[S[_], T](decoder: Decoder[S, T]): Decoder[Collection[S, *], Seq[T]] =
//     new CollectionDecoder(decoder)
