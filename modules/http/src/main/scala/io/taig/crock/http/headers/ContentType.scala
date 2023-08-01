package io.taig.crock.http.headers

import cats.syntax.all.*

final case class ContentType(mediaType: MediaType, charset: Option[String]):
  def render: String = (List(mediaType.toString) ++ charset.map(charset => s"charset=$charset")).mkString("; ")

object ContentType:
  def parse(value: String): Option[ContentType] = value.split(";\\s*").toList match
    case mediaType :: tail =>
      val charset = tail
        .map(_.split("\\s*=\\s*", 2))
        .collectFirst { case Array(name, value) if name.equalsIgnoreCase("charset") => value }
        .map(charset => if charset.startsWith("\"") && charset.endsWith("\"") then charset.tail.init else charset)

      ContentType(MediaType(mediaType), charset).some
    case Nil => none

  val validation: Validation[String, String, String, ContentType] =
    Validation.fromOptionNec(Constraint.parser("contentType"))(ContentType.parse)
