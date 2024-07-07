package io.taig.otter.http

import io.taig.otter as Base
import io.taig.otter.http as Http

trait Types extends Base.Types:
  val container: Container

  type Header[A] = container.Header[Http.Header[container.Schema, A]]

  object Header:
    type Reader[A] = container.Header[Http.Header.Reader[container.Schema, A]]

    type Writer[A] = container.Header[Http.Header.Writer[container.Schema, A]]

  type Headers[A] = container.Headers[Http.Headers[container.Schema, A]]

  object Headers:
    type Reader[A] = container.Headers[Http.Headers.Reader[container.Schema, A]]

    type Writer[A] = container.Headers[Http.Headers.Writer[container.Schema, A]]
