package io.taig.otter.codec

import cats.data.Validated
import io.taig.otter.Union
import io.taig.otter.Violations

@SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
final class UnionDecoder[F[- _, + _], T](decoder: Decoder[F, T]) extends Decoder[[w, r] =>> Union[F, w, r], T]:
  override def decode[R](schema: Union[F, Nothing, R], value: T): Validated[Violations, R] = (schema: @unchecked) match
    case schema: Union.Coproduct[F, ?, ?, ?, ?] =>
      decode(schema.left, value)
        .map(Left(_))
        .orElse(decode(schema.right, value).map(Right(_)))
        .map(_.asInstanceOf[R])
    case schema: Union.Modify[F, ?, ?, ?, R] => decode(schema.self, value).map(schema.f)
    case schema: Union.Root[F, ?, R]          => decoder.decode(schema.branch.value, value)
