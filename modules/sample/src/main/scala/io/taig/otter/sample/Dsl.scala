package io.taig.otter.sample

import io.taig.otter.http.json as HttpJson
import org.typelevel.ci.*
import java.util.regex.Pattern
import cats.syntax.all.*

object Dsl extends HttpJson.Dsl:
  val email: Primitive.Required[CIString] = cistring(matches = Pattern.compile(".+@.+", Pattern.CASE_INSENSITIVE).some)
