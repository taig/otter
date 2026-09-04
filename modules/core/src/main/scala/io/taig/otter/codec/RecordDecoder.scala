package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Record
import io.taig.otter.Violations

final class RecordDecoder[F[-_, +_], T](decoder: Decoder.Remaining[F, Fields[T]])
    extends Decoder.Remaining[[w, r] =>> Record[F, w, r], Fields[T]]:
  override def decodeRemaining[R](
      schema: Record[F, Nothing, R],
      values: Fields[T]
  ): Validated[Violations, (Fields[T], R)] = schema match
    case Record.Empty => (values, ()).valid
    // Every `:*` builds exactly this, so reading a member's pair and then rebuilding it one step later is the shape a
    // record is always in. Fusing the two applies `f` where the pair is made and never returns the pair itself, which
    // is a tuple per member that escapes up a recursive call and so is one escape analysis cannot reach.
    case Record.Modify(Record.Product(left, right), f, _) => product(left, right, values, f)
    case Record.Modify(self, f, _)                        => decodeRemaining(self, values).map(_.map(f))
    case Record.Product(left, right)                      => product(left, right, values, identity)
    case Record.Root(field)                               => decoder.decodeRemaining(field.value, values)

  /** Both sides, and what the record makes of the pair.
    *
    * `f` is taken here rather than applied to the result, because every `:*` wraps a [[Record.Product]] in a
    * [[Record.Modify]] and the two together would otherwise build the pair, return it, and rebuild it one step later.
    * The pair a member returns escapes up a recursive call, which is exactly the allocation escape analysis cannot
    * reach, so the one that is never returned is the one worth not making.
    *
    * The left side is decoded even when it has already failed, because that is what accumulates: a record reports every
    * member that is wrong, not the first.
    */
  private def product[R1, R2, R](
      left: Record[F, Nothing, R1],
      right: Record[F, Nothing, R2],
      values: Fields[T],
      f: ((R1, R2)) => R
  ): Validated[Violations, (Fields[T], R)] =
    decodeRemaining(left, values) match
      case Validated.Valid((values, a)) =>
        decodeRemaining(right, values) match
          case Validated.Valid((values, b))  => Validated.Valid((values, f((a, b))))
          case result @ Validated.Invalid(_) => result
      case result @ Validated.Invalid(l) =>
        decodeRemaining(right, values) match
          case Validated.Valid(_)   => result
          case Validated.Invalid(r) => Validated.Invalid(l |+| r)
