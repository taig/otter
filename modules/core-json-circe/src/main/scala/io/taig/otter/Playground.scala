package io.taig.otter

object Playground:
  import Codecs.*

  @main
  def run = {
    println(CirceJsonCodecPrinter(dynamic.value, Data.Array(List(1, 2, 3))))
  }
