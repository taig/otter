package io.taig.otter.http

import io.taig.otter as Base

object Plain extends Types:
  override object container extends Container:
    export Base.Plain.container.*
    override type Header[A] = A
    override type Query[A] = A
    override type Segment[A] = A
    override type Request[A] = A
    override type RequestBody[A] = A
