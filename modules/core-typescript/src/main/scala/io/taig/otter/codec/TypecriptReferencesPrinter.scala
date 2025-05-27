package io.taig.otter.codec

import io.taig.otter.Typescript

import scala.collection.immutable.ListMap

object TypecriptReferencesPrinter:
  def print(references: ListMap[String, Typescript]): List[String] =
    references.toList.map((name, value) => s"export type $name = $value")
