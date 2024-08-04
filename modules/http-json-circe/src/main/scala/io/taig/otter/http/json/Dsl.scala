package io.taig.otter.http.json

import io.taig.otter.http as Http

trait Dsl extends Http.Dsl, Codecs

object Dsl extends Dsl
