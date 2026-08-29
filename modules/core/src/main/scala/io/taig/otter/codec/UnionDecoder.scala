package io.taig.otter.codec

import cats.data.Validated
import io.taig.otter.Union
import io.taig.otter.Violations

final class UnionDecoder[F[-_, +_], T](decoder: Decoder[F, T]) extends Decoder[[w, r] =>> Union[F, w, r], T]:
  override def decode[R](schema: Union[F, Nothing, R], value: T): Validated[Violations, R] = schema match
    case Union.Coproduct(left, right) => either(left, right, value)
    case Union.Modify(self, f, _)     => decode(self, value).map(f)
    case Union.Root(branch)           => decoder.decode(branch.value, value)

  /** Naming both sides is what lets `orElse` see `Either[R1, R2]` as the result. */
  private def either[R1, R2](
      left: Union[F, Nothing, R1],
      right: Union[F, Nothing, R2],
      value: T
  ): Validated[Violations, Either[R1, R2]] =
    decode(left, value).map(Left(_): Either[R1, R2]).orElse(decode(right, value).map(Right(_)))
