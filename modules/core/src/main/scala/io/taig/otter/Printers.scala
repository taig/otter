package io.taig.otter

import cats.data.Chain
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.syntax.all.*
import io.taig.otter.Data.given

private[otter] object Printers:
  def apply(step: Step): String = step match
    case Step.Field(field) => s".$field"
    case Step.Index(index) => s"[$index]"

  def apply(xpath: XPath): String = "$" + xpath.toChain.map(Printers.apply).mkString_("")

  def apply(violations: Indexed[NonEmptyChain[Violation]]): String =
    val path = violations.xpath.show
    violations.self.map(violation => show"$path: $violation").mkString_("\n")

  def apply(violations: Violations): NonEmptyList[String] = violations.toNel.map(Printers.apply)
