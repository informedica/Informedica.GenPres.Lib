
type Test = { Number : int }

module Test =

    module Operators =

        let (<?) (t1: Test) (t2: Test) = t1.Number < t2.Number


let t1 = { Number = 1 }
let t2 = { Number = 2 }

open Test.Operators

t1 <? t2
