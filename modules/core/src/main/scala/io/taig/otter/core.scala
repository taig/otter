package io.taig.otter

import cats.Eq
import cats.syntax.all.*

import java.util.regex.Pattern
import cats.data.Chain

private[otter] given Eq[Data.Number] = Eq.fromUniversalEquals
private[otter] given Eq[Data.Primitive] = Eq.fromUniversalEquals
private[otter] given Eq[Data.Value] = Eq.fromUniversalEquals
private[otter] given Eq[Data] = Eq.fromUniversalEquals

private[otter] given Eq[Pattern] = Eq.by(_.pattern)

private[otter] def indent(value: String): String = value.split("\n").map("  " + _).mkString("\n")

private[otter] def escape(value: String, characters: List[String], escape: Char = '\\'): String =
  characters.foldLeft(value.replace(s"$escape", s"$escape$escape")): (value, character) =>
    value.replace(character, s"$escape$character")

private[otter] def escape(value: String, character: String): String = escape(value, characters = List(character))

private[otter] def unescape(value: String, characters: List[String], escape: Char = '\\'): String =
  characters
    .foldLeft(value): (value, character) =>
      value.replace(s"$escape$character", character).replace(s"$escape$escape", s"$escape")
    .replace(s"$escape$escape", s"$escape")

private[otter] def unescape(value: String, character: String): String =
  unescape(value, characters = List(character))

extension [A: Eq, B](self: Vector[(A, B)])
  private[otter] def filterKeys(keys: Iterable[A]): (Vector[(A, B)], Vector[(A, B)]) =
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

extension [A](self: List[A])
  private[otter] def collectFirstWithRemainders[B](pf: PartialFunction[A, B]): (List[A], Option[B]) =
    @SuppressWarnings(Array("scalafix:DisableSyntax.var"))
    var result: Option[B] = none
    val remainders = List.newBuilder[A]

    self.foreach: a =>
      if result.isEmpty && pf.isDefinedAt(a)
      then result = pf.apply(a).some
      else remainders += a

    if result.isEmpty
    then (self, none)
    else (remainders.result(), result)

extension [A](self: Chain[A])
  private[otter] def collectFirstWithRemainders[B](pf: PartialFunction[A, B]): (Chain[A], Option[B]) =
    self.toList.collectFirstWithRemainders(pf).leftMap(Chain.fromSeq)
