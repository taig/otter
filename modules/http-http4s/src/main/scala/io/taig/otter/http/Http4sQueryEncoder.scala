package io.taig.otter.http
import io.taig.otter.Encoder
import io.taig.otter.http.HttpKeys.*
import org.http4s.Query as Http4sQuery

object Http4sQueryEncoder extends Encoder[Query, Http4sQuery]:
  override def apply[A](query: Query[A], a: A): Http4sQuery = query match
    case Query.Modify(self, _, g) => apply(query = self, g(a))
    case Query.Optional(self)     => a.fold(Http4sQuery.empty)(apply(query = self, _))
    case Query.Root(name, codec, metadata) =>
      val values = HttpQueryPrinter(
        name,
        codec = codec.value,
        a,
        explode = metadata.get(explode).getOrElse(true),
        style = metadata.get(style).collect { case style: Query.Style => style }.getOrElse(Query.Style.Form)
      )

      Http4sQuery.fromVector(values.toVector)
