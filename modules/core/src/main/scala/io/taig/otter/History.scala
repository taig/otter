package io.taig.otter

import cats.Order
import cats.data.Chain

import java.util.regex.Pattern

opaque type History = Chain[History.Step]

object History:
  enum Step:
    case Field(name: String)
    case Index(value: Int)

  object Step:
    given Order[Step] =
      case (Field(x), Field(y)) => x compare y
      case (Index(x), Index(y)) => x compare y
      case (Field(_), Index(_)) => 1
      case (Index(_), Field(_)) => -1

  private val Parser = Pattern.compile("(?:\\.(\\w+))|\\[(\\d+)\\]")

  val Root: History = Chain.empty

  extension (history: History)
    def toChain: Chain[History.Step] = history
    def toList: List[History.Step] = history.toList
    def /(field: String): History = append(History.Step.Field(field))
    def /(index: Int): History = append(History.Step.Index(index))
    def /(step: History.Step): History = append(step)
    infix def append(step: History.Step): History = history.append(step)
    infix def prepend(step: History.Step): History = history.prepend(step)
    def initLast: Option[(History, History.Step)] = history.initLast
    def ++(right: History): History = history ++ right
    def isRoot: Boolean = history.isEmpty

    def up: History = history.initLast match
      case Some((init, _)) => init
      case None            => Root

    def toJsonPath: String = if history.isEmpty then "."
    else
      history.foldLeft(""):
        case (result, Step.Field(name))  => s"$result.$name"
        case (result, Step.Index(index)) => s"$result[$index]"

    def toString: String = toJsonPath

  extension (self: History.Step) def /:(history: History): History = history.prepend(self)
  extension (self: String) def /:(history: History): History = Step.Field(self) /: history
  extension (self: Int) def /:(history: History): History = Step.Index(self) /: history

  def parse(value: String): Either[String, History] =
    if value.isEmpty then Left("Empty")
    else if value == "." then Right(Root)
    else {
      val result = List.newBuilder[Step]
      val matcher = Parser.matcher(value)
      var lastOffset = 0

      try {
        while matcher.find() do {
          val field = matcher.group(1)
          val index = matcher.group(2)

          if matcher.start() > lastOffset then throw new IllegalArgumentException("Contains invalid characters")
          else lastOffset = matcher.end()

          if field != null then result += Step.Field(field)
          else result += Step.Index(index.toInt)
        }

        if lastOffset < value.length then throw new IllegalArgumentException("Contains invalid characters")

        Right(Chain.fromSeq(result.result()))
      } catch {
        case _: NumberFormatException            => Left("Invalid index format")
        case exception: IllegalArgumentException => Left(exception.getMessage)
      }
    }

  object `/`:
    def unapply(history: History): Option[(History, History.Step)] = Chain.:==.unapply(history)

  given (using order: Order[Chain[History.Step]]): Order[History] = order
