// package io.taig.otter

// import cats.data.Validated
// import cats.syntax.all.*

// final class DictionaryParser[S[_]](parser: Parser[S]):
//   def apply[A](codec: Dictionary[S, S, A], values: List[(String, String)]): Validated[Violations, A] = codec match
//     case Dictionary.Root(key, codec, _, _, _) =>
//       values.traverse: (name, value) =>
//         (parser(codec = key.value, name), parser(codec = codec.value, value)).tupled.leftMap(name /: _)
//     case Dictionary.Modify(self, f, _) => apply(codec = self, values).map(f)
