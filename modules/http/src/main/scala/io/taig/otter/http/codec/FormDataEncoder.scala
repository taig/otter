package io.taig.otter.http.codec

import io.taig.otter.codec.Encoder
import io.taig.otter.http.FormData

object FormDataEncoder extends Encoder[FormData, List[(String, Option[String])]]:
  override def encode[A](schema: FormData[A], a: A): List[(String, Option[String])] = ???
