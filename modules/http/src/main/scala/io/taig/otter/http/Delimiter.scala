// package io.taig.otter.http

// final case class Delimiter(delimiterValue: String, escapeCharacter: Char):
//   def escape(value: String): String = value
//     .replace(s"$escapeCharacter", s"$escapeCharacter$escapeCharacter")
//     .replace(delimiterValue, s"$escapeCharacter$delimiterValue")

//   def unescape(value: String): String = value
//     .replace(s"$escapeCharacter$delimiterValue", delimiterValue)
//     .replace(s"$escapeCharacter$escapeCharacter", s"$escapeCharacter")

//   def decode(value: String): Vector[String] = value.split(delimiterValue).toVector.map(unescape)

//   def encode(values: Vector[String]): Option[String] =
//     Some(values).filter(_.nonEmpty).map(_.map(escape).mkString(delimiterValue))

// object Delimiter:
//   val Default: Delimiter = Delimiter(delimiterValue = ",", escapeCharacter = '\\')
