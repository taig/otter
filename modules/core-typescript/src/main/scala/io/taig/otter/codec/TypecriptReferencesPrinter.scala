package io.taig.otter.codec

import scala.collection.immutable.ListMap
import io.taig.otter.Typescript

object TypecriptReferencesPrinter:
  def print(references: ListMap[String, Typescript]): List[String] =
    references.toList.map((name, value) => s"export type $name = $value")
