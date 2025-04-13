package io.taig.otter

import cats.syntax.all.*
import cats.data.Validated

final class RecordParser[S[_]](parser: Parser[S], printer: Printer[S]):
  def apply[A](
      codec: Record[S, S, A],
      values: List[(String, String)]
  ): Validated[Violations, (List[(String, String)], A)] = codec match
    case Record.Empty(_) => (values, ()).valid
    case Record.Field(key, value, _) =>
      val name = printer(codec = key.self.value, key.value)
      val (remainders, result) = collectFirstWithRemainders(values) { case (`name`, value) => value }

      result
        .toValid(Violations.rootNec(Violation.required))
        .andThen(parser(codec = value.self.value, _))
        .leftMap(name /: _)
        .tupleLeft(remainders)
    case Record.Modify(self, f, _) => apply(codec = self, values).map(_.map(f))
    case Record.Optional(self) =>
      val keys = self.fields.map((key, _) => printer(codec = key.self.value, key.value))
      val references = values.map((key, _) => key).toSet

      if keys.forall(!references.contains(_))
      then (values, none).valid
      else apply(codec = self, values).map(_.map(_.some))
    case Record.Zip(left, right, _) =>
      apply(codec = left, values) match
        case Validated.Valid((values, a)) =>
          apply(codec = right, values) match
            case Validated.Valid((values, b))      => (values, (a, b)).valid
            case violations @ Validated.Invalid(_) => violations
        case Validated.Invalid(left) =>
          apply(codec = right, values) match
            case Validated.Valid((_, _))           => left.invalid
            case violations @ Validated.Invalid(_) => violations
