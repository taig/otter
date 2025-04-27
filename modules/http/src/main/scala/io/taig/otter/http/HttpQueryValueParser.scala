package io.taig.otter.http

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.*

object HttpQueryValueParser extends Parser[Http.Query.Value]:
  override def apply[A](codec: Http.Query.Value[A], value: String): Validated[Violations, A] =
    codec match
      case Http.Query.Value.Constant(self)    => apply(codec = self, value)
      case Http.Query.Value.Enumeration(self) => apply(codec = self, value)
      case Http.Query.Value.Primitive(self)   => PrimitiveParser.Unquoted(codec = self, value)
      case Http.Query.Value.Union(self)       => apply(codec = self, value)

  def apply[A](codec: Constant[Http.Query.Value.Primitive, A], value: String): Validated[Violations, A] =
    codec match
      case Constant.Modify(self, f, _) => apply(codec = self, value).map(f)
      case Constant.Root(codec, _) =>
        val reference = PrimitivePrinter.Unquoted(codec = codec.self.value.self, codec.value)
        Validated.cond(
          test = value === reference,
          codec.value,
          Violations.rootNec(Violation.equal(reference, actual = value))
        )

  def apply[A](codec: Enumeration[Http.Query.Value.Primitive, A], value: String): Validated[Violations, A] =
    codec match
      case Enumeration.Modify(self, f, _) => apply(codec = self, value).map(f)
      case self @ Enumeration.Root(codec, mapping, _) =>
        apply(codec = codec.value, value).andThen: a =>
          mapping
            .unapply(a)
            .toValid:
              val values = self.values.map(mapping.apply).map(HttpQueryValuePrinter(codec.value, _))
              Violations.rootNec(Violation.oneOf(values = values.toList, actual = value))

  def apply[A](codec: Union.Untagged[Http.Query.Value, A], value: String): Validated[Violations, A] = codec match
    case Union.Untagged.Branch(_, codec, _) => apply(codec = codec.value, value)
    case Union.Untagged.Modify(self, f, _)  => apply(codec = self, value).map(f)
    case Union.Untagged.OrElse(left, right, _) =>
      apply(codec = left, value).map(Left(_)).findValid(apply(codec = right, value).map(Right(_)))
