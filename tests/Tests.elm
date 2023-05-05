module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import RingBuffer
import Test exposing (..)


tests : Test
tests =
    describe "RingBuffer"
        [ test "''An empty ring buffer |> isEmpty'' should be True." <|
            \_ ->
                RingBuffer.create 3 ""
                    |> RingBuffer.isEmpty
                    |> Expect.equal True
        , test "''Ring buffer with some element |> isEmpty'' should be False" <|
            \_ ->
                RingBuffer.create 3 ""
                    |> RingBuffer.enqueue "hoge"
                    |> RingBuffer.isEmpty
                    |> Expect.equal False
        , test "''An empty ring buffer |> isFull'' should be false" <|
            \_ ->
                RingBuffer.create 3 ""
                    |> RingBuffer.isFull
                    |> Expect.equal False
        , test "''3 bits ring buffer with 4 element |> isFull'' should be False" <|
            \_ ->
                RingBuffer.create 3 0
                    |> RingBuffer.enqueue 0
                    |> RingBuffer.enqueue 1
                    |> RingBuffer.enqueue 2
                    |> RingBuffer.enqueue 3
                    |> RingBuffer.isFull
                    |> Expect.equal False
        , test "''2 bits ring buffer enqueued with 4 element |> isFull'' should be True" <|
            \_ ->
                RingBuffer.create 2 0
                    |> RingBuffer.enqueue 0
                    |> RingBuffer.enqueue 1
                    |> RingBuffer.enqueue 2
                    |> RingBuffer.enqueue 3
                    |> RingBuffer.isFull
                    |> Expect.equal True
        , test "toList correctly working" <|
            \_ ->
                RingBuffer.create 3 ' '
                    |> RingBuffer.enqueueList
                        (String.toList "elm is great!")
                    |> RingBuffer.toList
                    |> Expect.equal [ ' ', 'g', 'r', 'e', 'a', 't', '!' ]
        , test "RingBuffer.length not cycled yet" <|
            \_ ->
                RingBuffer.create 3 ""
                    |> RingBuffer.enqueue "e"
                    |> RingBuffer.enqueue "l"
                    |> RingBuffer.enqueue "m"
                    |> RingBuffer.enqueue ""
                    |> RingBuffer.enqueue "i"
                    |> RingBuffer.enqueue "s"
                    |> RingBuffer.enqueue ""
                    |> RingBuffer.enqueue "g"
                    |> RingBuffer.enqueue "o"
                    |> RingBuffer.enqueue "o"
                    |> RingBuffer.enqueue "d"
                    |> RingBuffer.enqueue "!"
                    |> RingBuffer.length
                    |> Expect.equal 7
        , test "RingBuffer.length cycled" <|
            \_ ->
                RingBuffer.create 3 ""
                    |> RingBuffer.enqueue "e"
                    |> RingBuffer.enqueue "l"
                    |> RingBuffer.enqueue "m"
                    |> RingBuffer.length
                    |> Expect.equal 3
        ]
