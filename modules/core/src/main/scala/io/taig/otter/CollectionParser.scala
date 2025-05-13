// package io.taig.otter

// import cats.data.Validated
// import cats.syntax.all.*
// import codec.Collection

// final class CollectionParser[S[_]](parser: Parser[S]):
//   def apply[A](codec: Collection[S, A], values: List[String]): Validated[Violations, A] = codec match
//     case codec.Collection.Indexed(codec, _, _, _, _) =>
//       values.toVector.zipWithIndex
//         .traverse((value, index) => parser(codec = codec.value, value).leftMap(index /: _))
//     case codec.Collection.Linked(codec, _, _, _, _) =>
//       values.zipWithIndex
//         .traverse((value, index) => parser(codec = codec.value, value).leftMap(index /: _))
//     case codec.Collection.Modify(self, f, _) => apply(codec = self, values).map(f)
