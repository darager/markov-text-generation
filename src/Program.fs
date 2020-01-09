open System
open System.IO

let removeLineBreaks (text : string) =
    text.Replace ("\n", " ")

let splitText (text : string) =
    text.Split [|' '|]

let createWordPairs pairSize (words : string[]) =
    words |> Seq.windowed pairSize

let getWordPairs text pairSize =
    removeLineBreaks text
    |> splitText
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

let pairlength = 3
let text = File.ReadAllText "./example-file.txt"
let map = getMarkovMap text pairlength

let keys = getWordPairs text pairlength
           |> Seq.map splitWordPair
           |> Seq.map (fun (key,_) -> key)
           |> Seq.toArray

let rnd = Random()
let startPhrase = keys.[rnd.Next(keys.Length)]

let textLength = 100
let newText = splitText startPhrase |> Array.toList

let getPreviousWords words phraseLength =
    words
    |> Seq.windowed phraseLength
    |> Seq.last

let getRandomItem seq =
    let length = Seq.length seq
    seq |> Seq.item (rnd.Next length)

let getNextWord (map : Map<string,string list>) (previousWords : string[]) =
    joinWords previousWords
    |> map.TryGetValue
    |> fun (_,v) -> getRandomItem v

let appendNewWord nextWord newText =
    newText @ nextWord

let buildNewText map newText =
    getPreviousWords newText (pairlength-1)
    |> getNextWord map
    |> fun w -> newText @ [w]
    |> fun text -> (map, text)

let getNewText markovMap =
    (markovMap, newText)
    ||> buildNewText
    ||> buildNewText
    ||> buildNewText
    ||> buildNewText
    ||> buildNewText
    |> fun (_, text) -> text
    |> joinWords

getNewText map
|> printfn "%A"