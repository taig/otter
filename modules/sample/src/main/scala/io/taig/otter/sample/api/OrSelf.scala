package io.taig.otter.sample.api

enum ReferenceOrSelf[+A]:
  case Reference(value: A)
  case Self
