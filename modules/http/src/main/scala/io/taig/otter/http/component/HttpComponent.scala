package io.taig.otter.http.component

import io.taig.otter.http.Http

trait HttpComponent:
  object segment extends SegmentComponent.Dynamic[Http.Segment.Dynamic]:
    object parameter extends SegmentComponent.Parameter

object HttpComponent extends HttpComponent

object Playground:
  import HttpComponent.*

  val x = segment(name = "foobar", parameter = segment.parameter.string)
