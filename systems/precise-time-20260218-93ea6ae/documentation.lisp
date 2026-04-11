(in-package #:org.shirakumo.precise-time)

(docs:define-docs
  (type query-failed
    "Error signalled if a timer couldn't be fetched.")

  (variable PRECISE-TIME-UNITS-PER-SECOND
    "This is the number of units per second of the secondary return value of GET-PRECISE-TIME.

See GET-PRECISE-TIME")

  (variable MONOTONIC-TIME-UNITS-PER-SECOND
    "This is the number of units per second of the secondary return value of GET-MONOTONIC-TIME.

See GET-MONOTONIC-TIME")

  (function get-precise-time
    "Gets the current time with as much precision as possible.

Returns two values:
- A universal-time timestamp
- The number of sub-seconds in units of PRECISE-TIME-UNITS-PER-SECOND

The time returned *may* jump forwards or backwards if the system clock
is adjusted.

Signals an error of type QUERY-FAILED if the time could not be fetched
for whatever reason.

See PRECISE-TIME-UNITS-PER-SECOND
See GET-PRECISE-TIME/DOUBLE
See QUERY-FAILED")

  (function get-precise-time/double
    "Gets the current time in seconds as a double-float.

See GET-PRECISE-TIME")

  (function get-monotonic-time
    "Gets a monotonically increasing time with as much precision as possible.

Returns two values:
- A number of seconds since an arbitrary, but fixed epoch
- The number of sub-seconds in units of MONOTONIC-TIME-UNITS-PER-SECOND

The time returned does not jump forwards or backwards regardless of
system clock and will always remain monotonically increasing.

Signals an error of type QUERY-FAILED if the time could not be fetched
for whatever reason.

See MONOTONIC-TIME-UNITS-PER-SECOND
See GET-MONOTONIC-TIME/DOUBLE
See QUERY-FAILED")

  (function get-monotonic-time/double
    "Gets a monotonically increasing timestamp in seconds as a double-float.

See GET-MONOTONIC-TIME"))
