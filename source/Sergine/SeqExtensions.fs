module Seq

// Extension method for Seq similar to Seq.takeWhile
// but returns also the first element for which the predicate failed.
let takeWhileInclusive cond s =
    let notEmpty s = not (Seq.isEmpty s)
    seq {
        yield! s |> Seq.takeWhile cond
        let r = s |> Seq.skipWhile cond
        if notEmpty r then yield r |> Seq.head
    }
