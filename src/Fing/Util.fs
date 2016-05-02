// Copyright (c) 2010, Nathan Sanders
// Licence: New BSD. See accompanying documentation.
module Util

let curry f x y = f (x, y)

module Seq = 
    let butLast l = 
        let len = Seq.length l - 1
        Seq.take len l, Seq.skip len l |> Seq.head
