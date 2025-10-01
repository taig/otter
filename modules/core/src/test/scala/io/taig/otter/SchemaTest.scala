package io.taig.otter

import munit.FunSuite
import io.taig.otter.component.SchemaComponent.*

final class SchemaTest extends FunSuite:
  test("default") {
    field("foo", string)
  }
