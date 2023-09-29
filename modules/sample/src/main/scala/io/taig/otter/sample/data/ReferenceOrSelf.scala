package io.taig.otter.sample.data

enum ReferenceOrSelf[+A]:
  case Reference(value: A)
  case Self
