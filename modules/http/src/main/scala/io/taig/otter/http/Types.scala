package io.taig.otter.http

import io.taig.otter as Base
import io.taig.otter.http as Http

trait Types extends Base.Types:
  export Http.{
    App,
    Bodies,
    Body,
    Endpoint,
    Header,
    Headers,
    Queries,
    Query,
    Request,
    Response,
    Result,
    Results,
    Routes,
    Segment,
    Url
  }

object Types extends Types
