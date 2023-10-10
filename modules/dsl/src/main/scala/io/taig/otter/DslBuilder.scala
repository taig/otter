package io.taig.otter

import io.taig.otter as core

abstract class DslBuilder[A, B, C, D, E, F](
    val validations: B,
    val request: C,
    val response: D,
    val input: E,
    val output: F
):
  this: A =>

  export core.{
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

  export core.validation.{Constraint, History, Validation, Violation, Violations}

  export http.{
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
