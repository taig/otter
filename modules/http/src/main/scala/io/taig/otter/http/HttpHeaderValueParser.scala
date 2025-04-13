package io.taig.otter.http

import io.taig.otter.*
import cats.syntax.all.*
import cats.data.Validated

object HttpHeaderValueParser extends Parser[Http.Header.Value]:
  override def apply[A](codec: Http.Header.Value[A], value: String): Validated[Violations, A] = codec match
    case Http.Header.Value.Constant(self)    => apply(codec, value)
    case Http.Header.Value.Enumeration(self) => apply(codec, value)
    case Http.Header.Value.Primitive(self)   => PrimitiveParser.Unquoted(codec = self, value)
    case Http.Header.Value.Union(self)       => apply(codec, value)

  def apply[A](codec: Constant[Http.Header.Value.Primitive, A], value: String): Validated[Violations, A] =
    codec match
      case Constant.Modify(self, f, _) => apply(codec = self, value).map(f)
      case Constant.Root(codec, _) =>
        val reference = PrimitivePrinter.Unquoted(codec = codec.self.value.self, codec.value)
        Validated.cond(
          test = value === reference,
          codec.value,
          Violations.rootNec(Violation.equal(reference, actual = value))
        )

  def apply[A](codec: Enumeration[Http.Header.Value.Primitive, A], value: String): Validated[Violations, A] =
    codec match
      case Enumeration.Modify(self, f, g) => apply(codec = self, value).map(f)
      case self @ Enumeration.Root(codec, mapping, _) =>
        PrimitiveParser
          .Unquoted(codec = codec.value.self, value)
          .andThen: a =>
            mapping
              .unapply(a)
              .toValid:
                val values = self.values.toList
                  .map(mapping.apply)
                  .map(PrimitivePrinter.Unquoted(codec = codec.value.self, _))
                Violations.rootNec(Violation.oneOf(values, actual = value))

  def apply[A](codec: Union.Untagged[Http.Header.Value, A], value: String): Validated[Violations, A] = codec match
    case Union.Untagged.Branch(_, codec, _) => apply(codec = codec.value, value)
    case Union.Untagged.Modify(self, f, _)  => apply(codec = self, value).map(f)
    case Union.Untagged.OrElse(left, right, _) =>
      apply(codec = left, value).map(Left(_)).findValid(apply(codec = right, value).map(Right(_)))
