package io.taig.otter.http

import cats.data.Validated
import io.taig.otter.*
import cats.data.Chain
import cats.syntax.all.*

final class HttpQueryParser(explode: Boolean, style: Query.Style):
  def apply[A](name: String, codec: Http.Query[A], values: Chain[(String, Option[String])]): Validated[Violations, A] =
    codec match
      case codec: Http.Query.Array[A]    => ???
      case codec: Http.Query.Object[A]   => ???
      case codec: Http.Query.Optional[A] => apply(name, codec, values)
      case codec: Http.Query.Value[A]    => ??? // HttpQueryValueParser(codec, value)

  def apply[A](
      name: String,
      codec: Http.Query.Optional[A],
      values: Chain[(String, Option[String])]
  ): Validated[Violations, A] =
    apply(name, codec = codec.self, values)

  def apply[A](
      name: String,
      codec: Optional[Http.Query, A],
      values: Chain[(String, Option[String])]
  ): Validated[Violations, A] =
    codec match
      case Optional.Modify(self, f, _) => apply(name, codec = self, values).map(f)
      case Optional.Default(codec, default, _) =>
        if values.exists((key, _) => key === name)
        then apply(name, codec = codec.value, values)
        else default.valid
      case Optional.Nullable(codec, _) =>
        if values.exists((key, _) => key === name)
        then apply(name, codec = codec.value, values).map(_.some)
        else None.valid
