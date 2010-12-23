#!r6rs
(import (yuni scheme tests racket-r6rs arithmetic bitwise)
        (yuni scheme tests racket-r6rs test)
        (yuni scheme r6rs io simple))
(display "Running tests for (rnrs arithmetic bitwise)\n")
(run-arithmetic-bitwise-tests)
(report-test-results)
