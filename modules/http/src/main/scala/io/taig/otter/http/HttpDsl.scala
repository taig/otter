package io.taig.otter.http

import io.taig.otter.http.header.MediaTypeDsl

trait HttpDsl extends AppDsl, BodyDsl, EndpointDsl, RequestDsl, ResponseDsl, ResultsDsl, ResultDsl, SegmentDsl, UrlDsl:
  object code extends CodeDsl
  object header extends HttpHeaderDsl
  object method extends MethodDsl

  def formData: FormDataDsl = FormDataDsl

  val parameter: HttpParameterDsl = HttpParameterDsl

  val mediaType: MediaTypeDsl = MediaTypeDsl

object HttpDsl extends HttpDsl
