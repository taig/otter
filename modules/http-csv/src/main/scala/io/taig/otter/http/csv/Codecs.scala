package io.taig.otter.http.csv

import io.taig.otter.http as Http

trait Codecs extends Http.Types, Http.Codecs

object Codecs extends Codecs