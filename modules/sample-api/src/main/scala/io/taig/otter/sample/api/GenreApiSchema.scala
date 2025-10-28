package io.taig.otter.sample.api

import io.taig.otter.Dsl.json.*
import io.taig.otter.Dsl.*
import io.taig.otter.Json

enum GenreApiSchema:
  case Fiction
  case History
  case Romance

object GenreApiSchema:
  val json: Json.Enumeration[GenreApiSchema] = enumeration(string):
    case Fiction => "fiction"
    case History => "history"
    case Romance => "romance"
