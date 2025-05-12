package io.taig.otter.http

trait HttpDsl extends AppDsl, BodyDsl, EndpointDsl, RequestDsl, ResponseDsl, ResultsDsl, ResultDsl, SegmentDsl, UrlDsl:
  object code extends CodeDsl
  object header extends HttpHeaderDsl
  object method extends MethodDsl

  // val formData: FormDataDsl = FormDataDsl

  val parameter: HttpParameterDsl = HttpParameterDsl

object HttpDsl extends HttpDsl
