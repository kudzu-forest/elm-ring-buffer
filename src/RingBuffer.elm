module RingBuffer exposing (RingBuffer, clear, create, dequeue, enqueue, enqueueList, head, isEmpty, isFull, length, toList)

import Array exposing (Array)
import Bitwise


{-| A simple implementation of ring buffer with size = 2^m. The enqueue and dequeue operation and query about length takes O(1) time, while creation takes O(size) time. This package can be used as one way queue, if given sufficient value of m.
-}
type RingBuffer a
    = RingBuffer Int Int Int (Array a) -- RingBuffer bits writeIndex readIndex dataArray. writeIndex is always preceding readIndex.


{-| creates new vacant ring buffer with specified bit count of length (clamped into 2 - 16 bits) and default value a (never be used except for initialization).
Notice that ring buffer with n bit can contain 2^n - 1 elements, not 2^n.

    RingBuffer.create 2 0
        |> RingBuffer.enqueueList
            (List.range 0 100)
        |> RingBuffer.toList
        == [ 98, 99, 100 ]

-}
create : Int -> a -> RingBuffer a
create bitsCount a =
    let
        bits =
            Bitwise.shiftLeftBy (clamp 2 24 bitsCount) 1 - 1

        arr =
            Array.repeat (bits + 1) a
    in
    RingBuffer bits 0 0 arr


{-| checks if the ring buffer is empty

    RingBuffer.create 3 ""
        |> RingBuffer.isEmpty
        == True

    RingBuffer.create 3 ""
        |> RingBuffer.enqueue "first element"
        |> RingBuffer.isEmpty
        == False

    RingBuffer.create 3 ""
        |> RingBuffer.enqueue "first element"
        |> RingBuffer.dequeue
        |> RingBuffer.isEmpty
        == True

-}
isEmpty : RingBuffer a -> Bool
isEmpty (RingBuffer _ writeIndex readIndex _) =
    readIndex == writeIndex


{-| checks if the ring buffer is full.

    RingBuffer.create 2 0
        |> RingBuffer.enqueueList
            (List.range 0 100)
        |> RingBuffer.isFull
        == True

-}
isFull : RingBuffer a -> Bool
isFull (RingBuffer bits writeIndex readIndex _) =
    Bitwise.and bits (writeIndex + 1) == readIndex


{-| returns heading element of the ring buffer. Here "heading element" means "living oldest element".

    RingBuffer.create 2 0
        |> RingBuffer.head
        == Nothing

    RingBuffer.create 2 0
        |> RingBuffer.enqueueList
            (List.range 0 100)
        |> RingBuffer.head
        == Just 98

-}
head : RingBuffer a -> Maybe a
head (RingBuffer _ writeIndex readIndex arr) =
    if readIndex == writeIndex then
        Nothing

    else
        Array.get readIndex arr


{-| adds new content to the ring buffer. When the buffer is full, oldest content will be overwritten.

    RingBuffer.create 2 0
        |> RingBuffer.enqueue 0
        |> RingBuffer.enqueue 1
        |> RingBuffer.toList
        == [ 0, 1 ]

    RingBuffer.create 2 0
        |> RingBuffer.enqueue 0
        |> RingBuffer.enqueue 1
        |> RingBuffer.enqueue 2
        |> RingBuffer.enqueue 3
        |> RingBuffer.enqueue 4
        |> RingBuffer.toList
        == [ 2, 3, 4 ]

-}
enqueue : a -> RingBuffer a -> RingBuffer a
enqueue a (RingBuffer bits writeIndex readIndex arr) =
    let
        newWriteIndex =
            Bitwise.and bits (writeIndex + 1)

        newArr =
            Array.set writeIndex a arr
    in
    if Bitwise.and bits (writeIndex + 1) == readIndex then
        let
            newReadIndex =
                Bitwise.and bits (newWriteIndex + 1)
        in
        RingBuffer bits newWriteIndex newReadIndex newArr

    else
        RingBuffer bits newWriteIndex readIndex newArr


{-| removes oldest content from the ring buffer. When the buffer is empty, nothing happens.

    RingBuffer.create 10 0
        |> RingBuffer.enqueue 100
        |> RingBuffer.enqueue 200
        |> RingBuffer.enqueue 300
        |> RingBuffer.dequeue
        |> RingBuffer.toList
        == [ 200, 300 ]

    RingBuffer.create 10 0
        |> RingBuffer.dequeue
        |> RingBuffer.toList
        == RingBuffer.create 10 0
        |> RingBuffer.toList

-}
dequeue : RingBuffer a -> RingBuffer a
dequeue (RingBuffer bits writeIndex readIndex arr) =
    if readIndex == writeIndex then
        RingBuffer bits writeIndex readIndex arr

    else
        let
            newReadIndex =
                Bitwise.and bits (readIndex + 1)
        in
        RingBuffer bits writeIndex newReadIndex arr


{-| gets the whole list of elements in the queue.

    RingBuffer.create 10 0
        |> RingBuffer.enqueue 100
        |> RingBuffer.enqueue 200
        |> RingBuffer.enqueue 300
        |> RingBuffer.dequeue
        |> RingBuffer.toList
        == [ 200, 300 ]

-}
toList : RingBuffer a -> List a
toList rb =
    case head rb of
        Nothing ->
            []

        Just a ->
            a :: toList (dequeue rb)


{-| gets length of a ring buffer. Here "length" means how many elements are in the queue, not the whole buffer size.

    RingBuffer.create 10 0
        |> RingBuffer.enqueue 100
        |> RingBuffer.enqueue 200
        |> RingBuffer.enqueue 300
        |> RingBuffer.dequeue
        |> RingBuffer.length
        == 2

-}
length : RingBuffer a -> Int
length (RingBuffer bits writeIndex readIndex _) =
    Bitwise.and bits (writeIndex + bits + 1 - readIndex)


{-| takes in all elements in a list in left-to-right order.

    RingBuffer.create 2 0
        |> RingBuffer.enqueueList
            (List.range 0 100)
        |> RingBuffer.toList
        == [ 98, 99, 100 ]

-}
enqueueList : List a -> RingBuffer a -> RingBuffer a
enqueueList list rb =
    List.foldl enqueue rb list


{-| Deletes all elements in the queue.

    RingBuffer.create 3 0
        |> RingBuffer.enqueue 0
        |> RingBuffer.enqueue 1
        |> RingBuffer.enqueue 2
        |> RingBuffer.enqueue 3
        |> RingBuffer.enqueue 4
        |> RingBuffer.clear
        |> RingBuffer.toList
        == []

Note that this function is realized by changing pointer index of the ring queue, so internaly the deleted data is not cleared. So, comparison between two RingBuffers may cause unexpected result!

    RingBuffer.create 3 0
        |> RingBuffer.enqueue 1
        |> RingBuffer.clear
        == RingBuffer.create 3 0 --> False

Instead, you should compare after converting with `toList` function.

-}
clear : RingBuffer a -> RingBuffer a
clear (RingBuffer bits writeIndex readIndex arr) =
    RingBuffer bits writeIndex writeIndex arr
