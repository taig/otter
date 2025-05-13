package io.taig.otter.http

import io.taig.otter.http.header.MediaTypeDsl

trait HttpDsl extends AppDsl, BodyDsl, EndpointDsl, RequestDsl, ResponseDsl, ResultsDsl, ResultDsl, SegmentDsl, UrlDsl:
  lazy val code: CodeDsl = CodeDsl

  lazy val header: HttpHeaderDsl = HttpHeaderDsl

  lazy val method: MethodDsl = MethodDsl

  lazy val formData: FormDataDsl = FormDataDsl

  lazy val parameter: HttpParameterDsl = HttpParameterDsl

  lazy val mediaType: MediaTypeDsl = MediaTypeDsl

object HttpDsl extends HttpDsl
