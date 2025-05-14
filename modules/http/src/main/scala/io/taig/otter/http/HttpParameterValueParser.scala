package io.taig.otter.http

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.*
import io.taig.otter.http.Http.Parameter.Value
import io.taig.otter.schema.Constant

object HttpParameterValueParser extends Parser[Http.Parameter.Value]:
  override def apply[A](codec: Http.Parameter.Value[A], value: String): Validated[Violations, A] = codec match
    case Http.Parameter.Value.Constant(self)    => apply(codec = self, value)
    case Http.Parameter.Value.Enumeration(self) => apply(codec = self, value)
    case Http.Parameter.Value.Primitive(self)   => PrimitiveParser.Unquoted(codec = self, value)
    // case Http.Parameter.Value.Union(self)       => apply(codec = self, value)

  def apply[A](codec: Constant[Http.Parameter.Value.Primitive, A], value: String): Validated[Violations, A] =
    codec match
      case Constant.Modify(self, f, _) => apply(codec = self, value).map(f)
      case Constant.Root(codec, eq, _) =>
        PrimitiveParser
          .Unquoted(codec = codec.self.value.self, value)
          .andThen: a =>
            Validated.cond(
              test = eq.eqv(a, codec.value),
              (),
              Violations.rootNec(
                Violation.equal(
                  reference = PrimitivePrinter.Unquoted(codec = codec.self.value.self, codec.value),
                  actual = value
                )
              )
            )

  def apply[A](codec: Enumeration[Http.Parameter.Value.Primitive, A], value: String): Validated[Violations, A] =
    codec match
      case Enumeration.Modify(self, f, _) => apply(codec = self, value).map(f)
      case self @ Enumeration.Root(codec, mapping, _) =>
        apply(codec = codec.value, value).andThen: a =>
          mapping
            .unapply(a)
            .toValid:
              val values = self.values.map(mapping.apply).map(HttpParameterValuePrinter(codec.value, _))
              Violations.rootNec(Violation.oneOf(values = values.toList, actual = value))

  // def apply[A](codec: Union.Untagged[Http.Parameter.Value, A], value: String): Validated[Violations, A] = codec match
  //   case Union.Untagged.Branch(_, codec, _) => apply(codec = codec.value, value)
  //   case Union.Untagged.Modify(self, f, _)  => apply(codec = self, value).map(f)
  //   case Union.Untagged.OrElse(left, right, _) =>
  //     apply(codec = left, value).map(Left(_)).findValid(apply(codec = right, value).map(Right(_)))
