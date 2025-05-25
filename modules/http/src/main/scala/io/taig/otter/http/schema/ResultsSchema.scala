package io.taig.otter.http.schema

import io.taig.otter.schema.Schema

trait ResultsSchema[Self[+_[_], _]] extends SchemaK[Self]:
  self =>
