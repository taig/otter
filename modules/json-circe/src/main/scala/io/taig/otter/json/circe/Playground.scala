package io.taig.otter.json.circe

import io.taig.otter.Plain.*
import java.math.BigDecimal as JBigDecimal
import io.taig.otter.Plain

object Playground:
  @main
  def run = {
    println(JsonEncoder(string, "haha"))
    println(JsonEncoder(int, 42))
    println(JsonEncoder(long, 42))
    println(JsonEncoder(bigDecimal, JBigDecimal.valueOf(42)))
    println(JsonEncoder(string.toTuple, "hello"))
  }
