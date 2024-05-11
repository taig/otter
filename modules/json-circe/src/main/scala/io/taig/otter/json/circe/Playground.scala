package io.taig.otter.json.circe

import io.taig.otter.Type
import io.taig.otter.Primitive
import io.taig.otter.Plain.*
import java.math.BigDecimal as JBigDecimal

object Playground:
  @main
  def run = {
    println(JsonPrimitiveEncoder(string, "haha"))
    println(JsonPrimitiveEncoder(int, 42))
    println(JsonPrimitiveEncoder(long, 42))
    println(JsonPrimitiveEncoder(bigDecimal, JBigDecimal.valueOf(42)))
  }
