// package io.taig.otter

// import cats.data.Validated
// import cats.syntax.all.*

// final class TupleParser[S[_]](parser: Parser[S]):
//   def apply[A](codec: Tuple[S, A], values: List[String]): Validated[Violations, A] =
//     val reference = codec.codecs.size.toInt
//     val actual = values.size

//     Validated.cond(
//       test = actual <= reference,
//       (),
//       Violations.rootNec(Violation(Constraint.Collection.MaxItems(reference), actual, hint = none))
//     ) *> Validated.cond(
//       test = actual >= reference,
//       (),
//       Violations.rootNec(Violation(Constraint.Collection.MinItems(reference), actual, hint = none))
//     ) *> apply(codec, values, index = 0)

//   def apply[A](codec: Tuple[S, A], values: List[String], index: Int): Validated[Violations, A] =
//     codec match
//       case Tuple.Empty(_)           => ().valid
//       case Tuple.Modify(self, f, _) => apply(codec = self, values, index).map(f)
//       case Tuple.Root(codec, _) =>
//         values.headOption
//           .toValid(Violations.rootNec(Violation.required))
//           .andThen(parser(codec = codec.value, _))
//           .leftMap(index /: _)
//       case Tuple.Zip(left, right, _) =>
//         val size = left.codecs.size.toInt
//         values
//           .splitAt(size)
//           .bimap(apply(codec = left, _, index), apply(codec = right, _, index + size))
//           .tupled
