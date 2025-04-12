package io.taig.otter.http

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.*

final class HttpSegmentParser(explode: Boolean, style: Header.Style):
  def apply[A](name: String, codec: Http.Segment[A], value: String): Validated[Violations, A] = codec match
    case codec: Http.Segment.Array[A]  => ???
    case codec: Http.Segment.Object[A] => ???
    case codec: Http.Segment.Value[A]  => apply(name, codec, value)

  def apply[A](name: String, codec: Http.Segment.Value[A], value: String): Validated[Violations, A] = style
    .match
      case Header.Style.Simple => value.valid
      case Header.Style.Label =>
        if value.startsWith(".")
        then value.drop(1).valid
        else Violations.rootNec(Violation.tpe(name = "value", actual = value)).invalid
      case Header.Style.Matrix =>
        HttpSegmentParser.parser
          .value(value)
          .toValidated
          .leftMap: error =>
            Violations.rootNec(Violation.tpe(name = "value", actual = value, hint = error.show))
          .andThen: (key, value) =>
            if key === name
            then value.valid
            else Violations.rootNec(Violation.tpe(name = "value", actual = value)).invalid
    .andThen(apply(codec, _))

  def apply[A](codec: Http.Segment.Value[A], value: String): Validated[Violations, A] = codec match
    case Http.Segment.Value.Constant(self)    => apply(codec = self, value)
    case Http.Segment.Value.Enumeration(self) => apply(codec = self, value)
    case Http.Segment.Value.Primitive(self)   => PrimitiveParser.Unquoted(codec = self, value)
    case Http.Segment.Value.Union(self)       => apply(codec = self, value)

  def apply[A](codec: Constant[Http.Segment.Value.Primitive, A], value: String): Validated[Violations, A] = codec match
    case Constant.Modify(self, f, _) => apply(codec = self, value).map(f)
    case Constant.Root(codec, _) =>
      val reference = PrimitivePrinter.Unquoted(codec = codec.self.value.self, codec.value)
      Validated.cond(
        test = value === reference,
        codec.value,
        Violations.rootNec(Violation.equal(reference, actual = value))
      )

  def apply[A](codec: Enumeration[Http.Segment.Value.Primitive, A], value: String): Validated[Violations, A] =
    codec match
      case Enumeration.Modify(self, f, _) => apply(codec = self, value).map(f)
      case self @ Enumeration.Root(codec, mapping, _) =>
        apply(codec = codec.value, value).andThen: a =>
          mapping
            .unapply(a)
            .toValid:
              val values = self.values.map(mapping.apply).map(HttpSegmentPrinter(explode, style)(codec.value, _))
              Violations.rootNec(Violation.oneOf(values = values.toList, actual = value))

  def apply[A](codec: Union.Untagged[Http.Segment.Value, A], value: String): Validated[Violations, A] = codec match
    case Union.Untagged.Branch(_, codec, _) => apply(codec = codec.value, value)
    case Union.Untagged.Modify(self, f, _)  => apply(codec = self, value).map(f)
    case Union.Untagged.OrElse(left, right, _) =>
      apply(codec = left, value).map(Left(_)).findValid(apply(codec = right, value).map(Right(_)))

object HttpSegmentParser:
  private object parser:
    import cats.parse.Parser
    import cats.parse.Parser.*

    val escaped = (character: Char) =>
      charWhere(value => value != '\\' && value != character).orElse(char('\\') *> anyChar)

    val value =
      val parser = (char(';') *> escaped('=').rep0.string <* char('=')) ~ anyChar.rep0.string
      (value: String) => parser.parseAll(value).map(_.leftMap(unescape(_, "=")))
