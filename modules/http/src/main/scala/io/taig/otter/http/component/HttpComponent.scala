package io.taig.otter.http.component

import io.taig.otter.http.Http

trait HttpComponent:
  object query extends QueryComponent[Http.Query, Http.Query.Parameter]:
    object parameter extends QueryComponent.Parameter

  object segment extends SegmentComponent.Dynamic[Http.Segment.Dynamic, Http.Segment.Parameter]:
    object parameter extends SegmentComponent.Parameter

object HttpComponent extends HttpComponent

object Playground:
  import HttpComponent.*
  // import io.taig.otter.http.syntax.all.*

  val s = segment(name = "foobar", parameter = segment.parameter.string)
  val q = query(name = "foobar", parameter = query.parameter.string)
  // val _ = q :* q
