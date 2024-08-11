package io.taig.otter.sample.api

import io.taig.otter.sample.api.Dsl.*
import org.typelevel.ci.*
import java.util.regex.Pattern
import cats.syntax.all.*

trait Codecs:
  val email: Primitive.Required[CIString] = cistring(matches = Pattern.compile(".+@.+", Pattern.CASE_INSENSITIVE).some)
