package io.taig.otter.http

import cats.data.Validated
import io.taig.otter.Violations
import org.http4s.Entity as Http4sBody

final class Http4sBodyDecoder[F[_], S](decode: Array[Byte] => Validated[Violations, S]):
  def apply[A](body: Body[S, A], value: Http4sBody[F]): F[Validated[Violations, A]] = ???
