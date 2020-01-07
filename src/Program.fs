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

let splitWordPair (wordpair : string[]) =
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


let mapBuilder map words =
    let keyValuePair = splitWordPair words
    keyValuePair ||> updateMarkovMap map

let getMarkovMap text pairSize =
    getWordPairs text pairSize
    |> Seq.fold mapBuilder Map.empty



let text = File.ReadAllText "./example-file.txt"
let map = getMarkovMap text 4
printfn "%A" map