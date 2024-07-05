package io.taig.otter.json.circe

import io.circe.Json
import cats.data.Chain
import scala.collection.mutable.ListBuffer

def typeOf(json: Json): String = json.fold(
  "null",
  _ => "boolean",
  _ => "number",
  _ => "string",
  _ => "array",
  _ => "object"
)

extension [A](self: Chain[A])
  def findWithRemainders[B](pf: PartialFunction[A, B]): (Option[B], Chain[A]) =
    val remainders = ListBuffer.empty[A]
    var result: Option[B] = None

    self.iterator.foreach: a =>
      if (result.isDefined || !pf.isDefinedAt(a)) then remainders += a
      else result = pf.lift(a)

    (result, Chain.fromIterableOnce(remainders))
