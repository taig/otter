package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Union
import io.taig.otter.Violations

final class UnionDecoder[F[-_, +_], T](decoder: Decoder[F, T]) extends Decoder[[w, r] =>> Union[F, w, r], T]:
  override def decode[R](schema: Union[F, Nothing, R], value: T): Validated[Violations, R] = schema match
    case Union.Coproduct(left, right) => either(left, right, value)
    case Union.Modify(self, f, _)     => decode(self, value).map(f)
    case Union.Root(branch)           => decoder.decode(branch.value, value)

  /** The first branch that reads the value, or every branch's reason for refusing it.
    *
    * Nothing on the wire says which branch a document belongs to, so a union that matches nothing has no single place
    * to point at: the honest answer is what each branch wanted. `orElse` cannot give it -- it keeps the right operand's
    * violations and discards the left's, so a union of three reported only the third, under only the third's name, and
    * a caller was told about a branch that may have nothing to do with what they meant.
    *
    * Combining is what [[io.taig.otter.codec.BranchDecoder]] makes readable: it reports under the branch's own name, so
    * what comes back is one namespace per branch and the paths say which is which. A left match still short circuits,
    * so the right side is read only when the left has already failed.
    */
  private def either[R1, R2](
      left: Union[F, Nothing, R1],
      right: Union[F, Nothing, R2],
      value: T
  ): Validated[Violations, Either[R1, R2]] =
    decode(left, value).map(Left(_): Either[R1, R2]) match
      case valid: Validated.Valid[Either[R1, R2]] => valid
      case Validated.Invalid(refused)             =>
        decode(right, value).map(Right(_)).leftMap(refused |+| _)
