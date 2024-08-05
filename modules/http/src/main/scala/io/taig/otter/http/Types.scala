package io.taig.otter.http

import io.taig.otter as Base
import io.taig.otter.http as Http

trait Types extends Base.Types:
  export Http.{Endpoint, Header, Headers, Queries, Query, Request, Response, Result, Results, Url}

object Types extends Types
