package io.taig.otter.http

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.*
import io.taig.otter.Enumeration.Modify
import io.taig.otter.Enumeration.Root

object HttpHeaderParser:
  def apply[A](codec: Http.Header[A], value: String): Validated[Violations, A] = codec match
    case codec: Http.Header.Value[A] => apply(codec, value)

  def apply[A](codec: Http.Header.Value[A], value: String): Validated[Violations, A] = codec match
    case Http.Header.Value.Constant(self)    => apply(codec, value)
    case Http.Header.Value.Enumeration(self) => apply(codec, value)
    case Http.Header.Value.Primitive(self)   => PrimitiveParser.Unquoted(codec = self, value)
    case Http.Header.Value.Union(self)       => ???

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
