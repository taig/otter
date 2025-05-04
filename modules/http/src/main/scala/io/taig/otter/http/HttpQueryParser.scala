package io.taig.otter.http

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.*

final class HttpQueryParser(explode: Boolean, style: Query.Style):
  def apply[A](
      name: String,
      codec: Http.Query[A],
      values: List[(String, Option[String])]
  ): Validated[Violations, (List[(String, Option[String])], A)] = codec match
    case codec: Http.Query.Array[A]    => ???
    case codec: Http.Query.Object[A]   => ???
    case codec: Http.Query.Optional[A] => apply(name, codec, values)
    case codec: Http.Query.Value[A]    => apply(name, codec, values)

  def apply[A](
      name: String,
      codec: Http.Query.Optional[A],
      values: List[(String, Option[String])]
  ): Validated[Violations, (List[(String, Option[String])], A)] = apply(name, codec = codec.self, values)

  def apply[A](
      name: String,
      codec: Nullable[Http.Query, A],
      values: List[(String, Option[String])]
  ): Validated[Violations, (List[(String, Option[String])], A)] = codec match
    case Nullable.Modify(self, f, _) => apply(name, codec = self, values).map(_.map(f))
    case Nullable.Default(codec, default, _) =>
      val (remainders, result) = values.collectFirstWithRemainders { case (`name`, value) => value }

      result
        .toValid(Violations.rootNec(Violation.required))
        .andThen:
          case Some(_) => apply(name, codec = codec.value, values)
          case None    => (remainders, default).valid
    case Nullable.Root(codec, _) =>
      val (remainders, result) = values.collectFirstWithRemainders { case (`name`, value) => value }

      result
        .toValid(Violations.rootNec(Violation.required))
        .andThen:
          case Some(_) => apply(name, codec = codec.value, values).map(_.map(_.some))
          case None    => (remainders, none).valid

  def apply[A](
      name: String,
      codec: Http.Query.Value[A],
      values: List[(String, Option[String])]
  ): Validated[Violations, (List[(String, Option[String])], A)] =
    val (remainders, result) = values.collectFirstWithRemainders { case (`name`, value) => value }

    result.flatten
      .toValid(Violations.rootNec(Violation.required))
      .andThen(HttpQueryValueParser(codec, _).tupleLeft(remainders))
      .leftMap(name /: _)
