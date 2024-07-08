package io.taig.otter.http

import io.taig.otter as Base
import io.taig.otter.http as Http

trait Types extends Base.Types:
  val container: Container

  export Http.Method

  type Header[A] = container.Header[Http.Header[container.Schema, A]]

  object Header:
    type Reader[A] = container.Header[Http.Header.Reader[container.Schema, A]]

    type Writer[A] = container.Header[Http.Header.Writer[container.Schema, A]]

  type Headers[A] = Http.Headers[container.Header, container.Schema, A]

  object Headers:
    type Reader[A] = Http.Headers.Reader[container.Header, container.Schema, A]

    type Writer[A] = Http.Headers.Writer[container.Header, container.Schema, A]

  type Path[A] = Http.Path[container.PathParameter, container.Schema, A]

  object Path:
    type Parameter[A] = container.PathParameter[Http.Path.Parameter[container.Schema, A]]

  type Query[A] = container.Query[Http.Query[container.Schema, A]]

  type Queries[A] = Http.Queries[container.Query, container.Schema, A]

  type Url[A] = Http.Url[container.PathParameter, container.Query, container.Schema, A]

  type Request[A] = container.Request[
    Http.Request[container.PathParameter, container.Query, container.Header, container.RequestBody, container.Schema, A]
  ]

  object Request:
    type Any = container.Request[
      Http.Request[
        container.PathParameter,
        container.Query,
        container.Header,
        container.RequestBody,
        container.Schema,
        ?
      ]
    ]

    type Body[A] = container.RequestBody[Http.Request.Body[container.Schema, A]]
