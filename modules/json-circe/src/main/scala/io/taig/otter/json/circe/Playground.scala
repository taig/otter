package io.taig.otter.json.circe

import io.taig.otter.Plain.*

object Playground:
  @main
  def run = {
    println(JsonEncoder(string, "hallo"))
    println(JsonEncoder(tuple(int), 3))
  }
