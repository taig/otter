package io.taig.otter.benchmark

import io.taig.otter.fixture.Shape
import io.taig.otter.fixture.json

/** The worst case a union has: the last branch, so every branch before it is decoded and thrown away first. */
class UnionBenchmark extends JsonBenchmark(json.shape, Shape.Triangle(base = 3, height = 4))
