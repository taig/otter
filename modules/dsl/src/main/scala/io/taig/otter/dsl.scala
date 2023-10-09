package io.taig.otter

import io.taig.otter.http.syntax as http
import io.taig.otter.http.circe.codecs as circe
import io.taig.otter.validation.validations

class dsl[A, B, C, D, E, F](val validations: B, val request: C, val response: D, val input: E, val output: F):
  this: A =>

  export io.taig.otter.{
    Branch,
    Codec,
    Collection,
    Coproduct,
    Data,
    Dictionary,
    Discriminator,
    Dynamic,
    Enumeration,
    Field,
    Null,
    Primitive,
    Product,
    Record,
    Union,
    Value
  }

  export io.taig.otter.http.{
    App,
    Client,
    Code,
    Endpoint,
    Header,
    Headers,
    HttpServer,
    MediaType,
    Method,
    Queries,
    Query,
    Request,
    Response,
    Result,
    Results,
    Route,
    Routes,
    Segment,
    Url
  }

object dsl
    extends dsl[
      codecs & http & circe,
      validations,
      http.request,
      http.response & circe.response,
      http.input & circe.input,
      http.output & circe.output
    ](
      validations,
      http.request,
      new http.response with circe.response,
      new http.input with circe.input,
      new http.output with circe.output
    )
    with codecs
    with http
    with circe
