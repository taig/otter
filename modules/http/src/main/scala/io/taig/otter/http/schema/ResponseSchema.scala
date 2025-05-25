package io.taig.otter.http.schema

import io.taig.otter.schema.Schema
import io.taig.otter.http.HttpExport.*

trait ResponseSchema[Self[+_[_], _]] extends SchemaK[Self]:
  extension [S[_], A](self: Self[S, A]) def modifyResults[B](f: Results[S, A] => Results[S, B]): Self[S, B]
