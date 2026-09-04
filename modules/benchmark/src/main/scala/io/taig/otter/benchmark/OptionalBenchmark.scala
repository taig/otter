package io.taig.otter.benchmark

import io.taig.otter.fixture.Note
import io.taig.otter.fixture.json

/** A record holding an optional field, which is the member that has to ask for its key before it can read it. */
class OptionalBenchmark extends JsonBenchmark(json.omittedTag, Note("Dune", tag = Some(42)))
