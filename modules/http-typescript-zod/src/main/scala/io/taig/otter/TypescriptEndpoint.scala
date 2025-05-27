package io.taig.otter

import cats.Show
import cats.Traverse
import cats.derived.*
import cats.syntax.all.*

final case class TypescriptEndpoint[A](
    input: A,
    marker: String,
    types: List[A],
    definition: String
) derives Traverse

object TypescriptEndpoint:
  given [A: Show]: Show[TypescriptEndpoint[A]] = endpoint => show"""${endpoint.marker}
                                                                   |
                                                                   |${endpoint.types.mkString_("\n\n")}
                                                                   |
                                                                   |${endpoint.definition}""".stripMargin
