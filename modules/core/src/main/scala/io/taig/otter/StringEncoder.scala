package io.taig.otter

object StringEncoder extends Encoder[Schema.Value, String] {
  override def encode[B](schema: Schema.Value[B], b: B): String = ???
}
