// package io.taig.otter.http.headers

// import cats.syntax.all.*
// import io.taig.otter.http.MediaType
// import io.taig.otter.validation.{validations, Validation}

// final case class ContentType(mediaType: MediaType, charset: Option[String]):
//   def print: String = (mediaType.print :: charset.map(charset => s"charset=$charset").toList).mkString("; ")

// object ContentType:
//   def parse(value: String): Option[ContentType] = value.split(";\\s*").toList match
//     case mediaType :: tail =>
//       MediaType
//         .parse(mediaType)
//         .map: mediaType =>
//           val charset = tail
//             .map(_.split("\\s*=\\s*", 2))
//             .collectFirst { case Array(name, value) if name.equalsIgnoreCase("charset") => value }
//             .map(charset => if charset.startsWith("\"") && charset.endsWith("\"") then charset.tail.init else charset)

//           ContentType(mediaType, charset)
//     case Nil => none

//   val validation: Validation[String, ContentType] = validations.parse("contentType")(parse)
