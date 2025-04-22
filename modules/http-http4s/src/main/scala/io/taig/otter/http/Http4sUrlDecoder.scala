package io.taig.otter.http

import io.taig.otter.Encoder
import org.http4s.Query as Http4sQuery
import org.http4s.Uri as Http4sUri
import org.http4s.syntax.all.*
import cats.data.Validated
import io.taig.otter.Violations

object Http4sUrlDecoder:
  def apply[A](url: Url[A], value: Http4sUri): Validated[Violations, A] = ???
