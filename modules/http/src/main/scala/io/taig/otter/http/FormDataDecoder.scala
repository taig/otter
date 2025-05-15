package io.taig.otter.http

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.*
import io.taig.otter.codec.PrimitiveParser

object FormDataDecoder:
  def apply[A](codec: FormData[A], data: List[(String, Option[String])]): Validated[Violations, A] = codec match
    case FormData.Dictionary(self) => apply(codec = self, data)
    case FormData.Record(self)     => apply(codec = self, data).map((_, a) => a)

  def apply[A](codec: FormData.Value[A], data: Option[String]): Validated[Violations, A] = codec match
    case FormData.Value.Nullable(self)  => apply(codec = self, data)
    case FormData.Value.Primitive(self) => apply(codec = self, data)

  def apply[A](codec: Nullable[FormData.Value, A], data: Option[String]): Validated[Violations, A] = codec match
    case Nullable.Modify(self, f, _) => apply(codec = self, data).map(f)
    case Nullable.Default(reference, default, _) =>
      if data.isEmpty then default.valid
      else apply(codec = reference.value, data)
    case Nullable.Root(reference, _) =>
      if data.isEmpty then None.valid
      else apply(codec = reference.value, data).map(_.some)
    case Nullable.Void(_) => ().valid

  def apply[A](codec: Primitive.String[A], data: Option[String]): Validated[Violations, A] = data
    .toValid(Violations.rootNec(Violation.tpe(name = "value", actual = Data.Null)))
    .andThen(PrimitiveParser(quotes = false)(codec, _))

  def apply[A](
      codec: Dictionary[FormData.Key, FormData.Value, A],
      data: List[(String, Option[String])]
  ): Validated[Violations, A] = codec match
    case Dictionary.Root(key, codec, minimum, maximum, _) =>
      val size = data.size

      minimum.traverse(minimum =>
        Validated.cond(
          test = size >= minimum,
          (),
          Violations.rootNec(
            Violation(constraint = Constraint.Object.MinProperties(reference = minimum), actual = size, hint = none)
          )
        )
      ) *> maximum.traverse(maximum =>
        Validated.cond(
          test = size <= maximum,
          (),
          Violations.rootNec(
            Violation(constraint = Constraint.Object.MaxProperties(reference = maximum), actual = size, hint = none)
          )
        )
      ) *> data.traverse: (name, value) =>
        (
          FromDataKeyParser(codec = key.value, value = name).leftMap(name /: _),
          apply(codec = codec.value, value).leftMap(name /: _)
        ).tupled
    case Dictionary.Modify(self, f, _) => apply(codec = self, data).map(f)

  def apply[A](
      codec: Record[FormData.Field, A],
      data: List[(String, Option[String])]
  ): Validated[Violations, (List[(String, Option[String])], A)] = ???
  // codec match
  //   case Record.Empty(_) => (data, ()).valid
  //   case Record.Field(key, codec, _) =>
  //     val name = FormDataKeyPrinter(codec = key.self.value, key.value)
  //     val (remainders, result) = data.collectFirstWithRemainders { case (`name`, json) => json }
  //     result
  //       .toValid(Violations.rootNec(Violation.tpe(name = "value", actual = "null")))
  //       .andThen(apply(codec = codec.value, _))
  //       .leftMap(name /: _)
  //       .tupleLeft(remainders)
  //   case Record.Modify(self, f, g) => apply(codec = self, data).map(_.map(f))
  //   case Record.Optional(self) =>
  //     val lookup = data.map((key, _) => key).toSet

  //     val allKeysAbsent = codec.fields
  //       .map((key, _) => FormDataKeyPrinter(codec = key.self.value, key.value))
  //       .forall(lookup.contains_)

  //     if allKeysAbsent then (data, none).valid else apply(codec = self, data).map(_.map(_.some))
  //   case Record.Zip(left, right, _) =>
  //     apply(codec = left, data) match
  //       case Validated.Valid((data, a)) => apply(codec = right, data).map(_.tupleLeft(a))
  //       case Validated.Invalid(violations) =>
  //         apply(codec = right, data).fold(violations |+| _, _ => violations).invalid
