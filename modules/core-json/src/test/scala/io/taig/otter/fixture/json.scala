package io.taig.otter.fixture

import io.taig.otter.Json
import io.taig.otter.component.JsonComponent.*

object json:
  val animal: Json.Enumeration[Animal] = enumeration(string):
    case Animal.Bird => "bird"
    case Animal.Cat  => "cat"
    case Animal.Dog  => "dog"
