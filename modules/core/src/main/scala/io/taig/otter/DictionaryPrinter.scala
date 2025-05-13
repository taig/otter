// package io.taig.otter

// import scala.annotation.tailrec

// final class DictionaryPrinter[S[_]](printer: Printer[S]):
//   @tailrec
//   def apply[A](codec: Dictionary[S, S, A], a: A): List[(String, String)] = codec match
//     case Dictionary.Root(key, codec, _, _, _) =>
//       a.map((name, value) => (printer(codec = key.value, name), printer(codec = codec.value, value)))
//     case Dictionary.Modify(self, f, g) => apply(codec = self, g(a))
