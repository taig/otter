package io.taig.otter

import scala.collection.immutable.Iterable
import cats.Eq
import cats.syntax.all.*
import cats.data.Chain

extension [A: Eq, B](self: Vector[(A, B)])
  def filterKeys(keys: Iterable[A]): (Vector[(A, B)], Vector[(A, B)]) =
    val remainingKeys = keys.toBuffer
    val result = Vector.newBuilder[(A, B)]
    val remainders = Vector.newBuilder[(A, B)]

    self.foreach { case value @ (key, _) =>
      if remainingKeys.exists(_ === key)
      then
        remainingKeys -= key
        result += value
      else remainders += value
    }

    (result.result(), remainders.result())

private def printHistory(history: Chain[Step]): String =
  val steps = history.map:
    case step @ Step.Field(_) => s".${step.print}"
    case step @ Step.Index(_) => step.print

  "$" + steps.mkString_("")
