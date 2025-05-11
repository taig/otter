package io.taig.otter.http

import cats.data.Chain

object QueryDataEncoder:
  def apply[A](query: Query[A], a: A): Queries.Data = query match
    case Query.Modify(self, _, g) => apply(query = self, g(a))
    case Query.Optional(self)     => a.map(apply(query = self, _)).getOrElse(Chain.empty)
    case Query.Root(name, codec, metadata) =>
      val explode = metadata.get(HttpKeys.explode).getOrElse(false) // TODO proper default
      val style = metadata
        .get(HttpKeys.style)
        .collect { case style: Query.Style => style }
        .getOrElse(Query.Style.Form)

      HttpQueryPrinter(explode, style)(name, codec = codec.value, a)
