package io.taig.otter.sample.fixtures

import io.taig.otter.sample.api.schema.IsbnApiSchema

def isbn(index: Int = 0): IsbnApiSchema = IsbnApiSchema.unsafe(9780763630188L + index)
