module Day25

// problem page
// http://adventofcode.com/2017/day/22

type Tape = {back: int list; forward : int list}

type Direction = Left | Right

let myTape = {back = []; forward = [0]}

let move direction tape =
    match direction with
    | Right ->
                match tape.forward with
                | [] -> {back = tape.back; forward = [0]}
                | [a] -> {back = a::tape.back; forward = [0]}
                | h::tail -> {back = h::tape.back; forward = tail}
    | Left -> 
                match tape.back with
                | [] -> {back = []; forward = 0::tape.forward}
                | [a] -> {back = []; forward = a::tape.forward}
                | h::tail -> {back = tail; forward = h::tape.forward}

let setValue value tape =
    match tape.forward with
    | [] -> {back = tape.back; forward = [value]}
    | [_] -> {back = tape.back; forward = [value]}
    | _::tail -> {back = tape.back; forward = value::tail}

let getValue tape = tape.forward |> List.head

type State = A | B | C | D | E | F     

let getStateAction state tape =
    match state with
    | A ->  if getValue tape = 0 
            then 1, Right, B
            else 0, Right, F      
    | B ->  if getValue tape = 0
            then 0, Left, B
            else 1, Left, C
    | C ->  if getValue tape = 0
            then 1, Left, D
            else 0, Right, C
    | D ->  if getValue tape = 0
            then 1, Left, E
            else 1, Right, A
    | E ->  if getValue tape = 0
            then 1, Left, F
            else 0, Left, D
    | F ->  if getValue tape = 0
            then 1, Right, A
            else 0, Left, E

let doStateAction (state, tape) =
    let value, direction, nextState = getStateAction state tape
    nextState, tape |> setValue value |> move direction     

let steps = 12964419 

let rec diagnostic n finish state tape =
    if n < finish then
        let state, tape = doStateAction (state, tape)   
        diagnostic (n + 1) finish state tape
    else
        tape  

let finalTape = diagnostic 0 steps A myTape      

let checkSum = (finalTape.forward |> List.sum) + (finalTape.back |> List.sum)
     