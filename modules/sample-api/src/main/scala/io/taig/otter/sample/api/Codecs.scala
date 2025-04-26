// package io.taig.otter.sample.api

// import cats.syntax.all.*
// import io.taig.otter.http as Http
// import org.typelevel.ci.*

// import java.util.regex.Pattern

// trait Codecs extends Http.Codecs:
//   val email: Primitive[CIString] = cistring(matches = Pattern.compile(".+@.+", Pattern.CASE_INSENSITIVE).some)
