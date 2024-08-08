package io.taig.otter.http.header

import cats.data.NonEmptyList
import cats.parse.Parser
import io.taig.otter.http.Parsers

opaque type Accept = NonEmptyList[Weighted[MediaRange]]

object Accept:
  def apply(values: NonEmptyList[Weighted[MediaRange]]): Accept = values

  def parse(value: String): Either[Parser.Error, Accept] = Parsers.accept.parseAll(value)
