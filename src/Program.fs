open System.IO

let splitText (text : string) =
    text.Split [|' '|]

let createWordPairs pairSize (words : string[]) =
    words |> Seq.windowed pairSize

let getWordPairs text pairSize =
    splitText text
    |> createWordPairs pairSize

let joinWords words =
    words |> String.concat " "

let splitWordPair (wordpair : string list) =
    let length = wordpair.Length
    let precedingWords = wordpair |> Seq.take (length-1)
    (joinWords precedingWords, wordpair.[length-1])

let updateMarkovMap (map : Map<_,_>) key value =
    if map.ContainsKey key then
        let previousValue = map.[key]
        let map = map |> Map.remove key
        map |> Map.add key (value :: previousValue)
    else
        map |> Map.add key [value]


let getMarkovMap pairSize text =
    Map


File.ReadAllText "./example-file.txt"
|> getMarkovMap 4
|> printfn "%A"