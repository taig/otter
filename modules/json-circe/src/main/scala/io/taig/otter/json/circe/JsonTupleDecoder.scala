package io.taig.otter.json.circe

import cats.syntax.all.*
import io.taig.otter.Tuple
import io.circe.Json
import cats.data.Chain
import cats.data.Validated
import io.taig.otter.validation.Violations

object JsonTupleDecoder:
  def decode[A](schema: Tuple[?, A], values: Option[Chain[Json]]): Validated[Violations[Json], A] = values match
    case Some(values) => decodeWithRemainders(schema, values).map { case (_, a) => a } // TODO error if not empty
    case None =>
      schema match
        case Tuple.Optional(_) => none.valid[Violations[Json]]
        case _                 => ??? // TODO ERROR: required

  def decodeWithRemainders[A](schema: Tuple[?, A], values: Chain[Json]): Validated[Violations[Json], (Chain[Json], A)] =
    schema match
      case Tuple.Empty(_)             => (Chain.empty, ()).valid
      case Tuple.Modify(schema, f, _) => decodeWithRemainders(schema, values).map(_.map(f))
      case Tuple.One(_, schema) =>
        values.uncons match
          case Some((head, tail)) => JsonDecoder.decode(schema, head).tupleLeft(tail)
          case None               => ???
      case Tuple.Optional(schema) => decodeWithRemainders(schema, values).map(_.map(_.some))
      case Tuple.Product(_, left, right) =>
        decodeWithRemainders(left, values).andThen { case (remainders, a) =>
          decodeWithRemainders(right, remainders).map(_.tupleLeft(a))
        }
