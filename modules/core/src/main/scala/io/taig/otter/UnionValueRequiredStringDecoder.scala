package io.taig.otter

object UnionValueRequiredStringDecoder:
  def apply[A](union: Union.Value.Required[?, A], value: String): Decoder.Result[Data, A] =
    ???
