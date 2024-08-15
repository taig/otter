package io.taig.otter.http.csv

import io.taig.otter.http.Body
import io.taig.otter.Product
import io.taig.otter.Data.Optional
import java.nio.charset.Charset
import io.taig.otter.http.header.MediaType
import io.taig.otter.Codec.Result
import io.taig.otter.Codec

// sealed abstract class Csv[A] extends Body.Encoded[A]:
//   override def codec: Product[?, ?, ?, ?] = ???

// object Csv:
//   def apply[A](mediaType: MediaType, of: Product[?, ?, ?, A]): Csv[Vector[A]] =
//     val _mediaType = mediaType

//     new Csv[Vector[A]]:
//       override def mediaType: MediaType = _mediaType
//       override def decode(charset: Option[Charset], payload: Array[Byte]): Codec.Result[Vector[A]] = ???
//       override def encode(charset: Option[Charset], a: Vector[A]): Array[Byte] = ???
