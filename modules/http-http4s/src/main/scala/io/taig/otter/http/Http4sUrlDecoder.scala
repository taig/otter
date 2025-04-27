package io.taig.otter.http
import cats.data.Validated
import io.taig.otter.Violations
import org.http4s.Query as Http4sQuery
import org.http4s.Uri as Http4sUri

object Http4sUrlDecoder:
  def apply[A](
      url: Url[A],
      path: Vector[Http4sUri.Path.Segment],
      queries: Vector[Http4sQuery.KeyValue]
  ): Validated[Violations, (Vector[Http4sUri.Path.Segment], Vector[Http4sQuery.KeyValue], A)] = ???
