package io.taig.otter.http

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Violations
import org.http4s.Query as Http4sQuery

object Http4sQueryDecoder:
  def apply[A](
      query: Query[A],
      values: List[Http4sQuery.KeyValue]
  ): Validated[Violations, (List[Http4sQuery.KeyValue], A)] = ???
  // query match
  //   case Query.Modify(self, f, _) => apply(query = self, values).map(_.map(f))
  //   case Query.Optional(self) =>
  //     if values.exists((key, _) => key === self.name)
  //     then apply(query = self, values).map(_.map(_.some))
  //     else (values, none).valid
  //   case Query.Root(name, codec, metadata) =>
  //     val explode = metadata.get(HttpKeys.explode).getOrElse(false) // TODO proper default
  //     val style =
  //       metadata.get(HttpKeys.style).collect { case style: Query.Style => style }.getOrElse(Query.Style.Form)
  //     new HttpQueryParser(explode, style)(name, codec = codec.value, values)
