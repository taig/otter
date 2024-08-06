package io.taig.otter.http

import cats.syntax.all.*
import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.nio.charset.Charset

opaque type FormData = Vector[(String, Option[String])]

object FormData:
  extension (self: FormData)
    def get(key: String): Vector[Option[String]] = self.collect { case (`key`, value) => value }
    def print(charset: Charset): String = self
      .map {
        case (key, Some(value)) =>
          s"${URLEncoder.encode(key, charset)}=${URLEncoder.encode(value, StandardCharsets.UTF_8)}"
        case (key, None) => URLEncoder.encode(key, charset)
      }
      .mkString("&")
    def toVector: Vector[(String, Option[String])] = self

  def apply(values: Vector[(String, Option[String])]): FormData = values

  def parse(value: String): FormData = value
    .split('&')
    .toVector
    .map: value =>
      value.split("=", 2) match
        case Array(key, value) => (key, value.some)
        case _                 => (value, none)
