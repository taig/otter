package io.taig.otter.http

import io.taig.otter.Violations
import cats.data.Validated
import cats.syntax.all.*

object QueryDataDecoder:
  object Remainders:
    def apply[A](query: Query[A], data: Queries.Data): Validated[Violations, (Queries.Data, A)] = query match
      case Query.Modify(self, f, _) => apply(query = self, data).map(_.map(f))
      case Query.Optional(self) =>
        if data.exists((key, _) => key === self.name)
        then apply(query = self, data).map(_.map(_.some))
        else (data, none).valid
      case Query.Root(name, codec, metadata) =>
        val explode = metadata.get(HttpKeys.explode).getOrElse(false) // TODO proper default
        val style = metadata
          .get(HttpKeys.style)
          .collect { case style: Query.Style => style }
          .getOrElse(Query.Style.Form)

        HttpQueryParser(explode, style)(name, codec = codec.value, data).leftMap(name /: _)
