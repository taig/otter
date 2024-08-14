package io.taig.otter.http.csv

import io.taig.otter.http as Http

trait Dsl extends Http.Dsl, Codecs

object Dsl extends Dsl
