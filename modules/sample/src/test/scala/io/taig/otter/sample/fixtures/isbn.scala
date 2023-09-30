package io.taig.otter.sample.fixtures

import io.taig.otter.sample.data.Isbn

def isbn(index: Int = 0): Isbn = Isbn.unsafeFromLong(9780763630188L + index)
