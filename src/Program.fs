open System
open System.IO

let removeLineBreaks (text : string) =
    text.Replace ("\n", " ")

let splitText (text : string) =
    text.Split [|' '|]

let createWordPairs pairSize (words : string[]) =
    words |> Seq.windowed pairSize

let getWordPairs pairSize text =
    text
    |> removeLineBreaks
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
    text
    |> getWordPairs pairSize
    |> Seq.fold mapBuilder Map.empty

let rnd = Random()
let getRandomItem seq =
    let length = Seq.length seq
    seq |> Seq.item (rnd.Next length)

let getPreviousWords words phraseLength =
    words
    |> Seq.windowed phraseLength
    |> Seq.last

let getNextWord (map : Map<_,_>) (previousWords : string[]) =
    previousWords
    |> joinWords
    |> map.TryGetValue
    |> fun (_,v) -> getRandomItem v

let buildNewText pairlength map newText =
    getPreviousWords newText (pairlength-1)
    |> getNextWord map
    |> fun w -> newText @ [w]
    |> fun text -> (map, text)

let appendNewWords markovMap generatedText pairlength amount =
    (markovMap, generatedText)
    ||> buildNewText pairlength
    ||> buildNewText pairlength
    ||> buildNewText pairlength
    ||> buildNewText pairlength
    ||> buildNewText pairlength
    |> fun (_, text) -> text
    |> joinWords


let pairlength = 3
let text = File.ReadAllText "./example-file.txt"
let map = getMarkovMap text pairlength

let keys = getWordPairs pairlength text
           |> Seq.map splitWordPair
           |> Seq.map (fun (key,_) -> key)
           |> Seq.toArray

let startPhrase = getRandomItem keys
let newText = splitText startPhrase |> Array.toList

appendNewWords map newText pairlength 100
|> printfn "%A"