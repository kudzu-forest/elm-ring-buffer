# elm-ring-buffer
A ring buffer queue with the simplest API.
Note that maximum count of elements in a ring buffer is restricted to $2^m - 1$,
where $m$ is specified at the creation.
`head`, `enqueue`, `dequeue`, and `length` takes $\varTheta(1)$ time.

    -- create ring buffer with size 2^2 - 1 initialized with 0.
    -- This initial value is needed only for initialization and will never be read.
    RingBuffer.create 2 0
      |> RingBuffer.enqueue 1
      |> RingBuffer.enqueue 2
      |> RingBuffer.enqueue 3
      |> RingBuffer.enqueue 4
      |> RingBuffer.enqueue 5
      |> RingBuffer.toList
      == [ 3, 4, 5 ]

