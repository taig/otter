package io.taig.otter.benchmark

import io.taig.otter.fixture.Census
import io.taig.otter.fixture.json

/** A record wide enough for a per member lookup over its own keys to be felt: fifteen fields over fifteen keys. */
class RecordBenchmark
    extends JsonBenchmark(
      json.census,
      Census(
        first = "first",
        second = "second",
        third = "third",
        fourth = "fourth",
        fifth = "fifth",
        sixth = "sixth",
        seventh = "seventh",
        eighth = "eighth",
        ninth = "ninth",
        tenth = "tenth",
        eleventh = "eleventh",
        twelfth = "twelfth",
        thirteenth = "thirteenth",
        fourteenth = "fourteenth",
        fifteenth = "fifteenth"
      )
    )
