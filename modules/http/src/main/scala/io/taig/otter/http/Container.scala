package io.taig.otter.http

import io.taig.otter as Base

trait Container extends Base.Container:
  type Header[+A]
  type Query[+A]
  type Segment[+A]
