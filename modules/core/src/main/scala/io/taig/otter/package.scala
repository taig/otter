package io.taig.otter

import cats.data.Chain
import cats.syntax.all.*

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
