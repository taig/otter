// package io.taig.otter

// import cats.data.Chain

// final class TuplePrinter[S[_]](printer: Printer[S]):
//   def apply[A](codec: Tuple[S, A], a: A): Chain[String] = codec match
//     case Tuple.Empty(_)            => Chain.empty
//     case Tuple.Modify(self, _, g)  => apply(codec = self, g(a))
//     case Tuple.Root(codec, _)      => Chain.one(printer(codec = codec.value, a))
//     case Tuple.Zip(left, right, _) => apply(codec = left, a._1) ++ apply(codec = right, a._2)
