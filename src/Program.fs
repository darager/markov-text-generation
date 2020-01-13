open System
open System.IO
open System.Text.RegularExpressions


let splitText (text : string) =
    text.Split [|' '|]
    |> Array.toList

let removeSpecialCharacters (text : string) =
    Regex.Replace (text, @"\\\r", " ")

let removeInvalidWords words =
    words
    |> List.map removeSpecialCharacters
    |> List.filter (String.IsNullOrEmpty >> not)

let joinWords = String.concat " "

let splitOffLastWord (wordpair : string[]) =
    let length = wordpair.Length
    let precedingWords = wordpair |> Seq.take (length-1)
    (precedingWords, wordpair.[length-1])

let concatPhrase (phrase, followingWord) =
    (joinWords phrase, followingWord)

let appendFirstWords n (words : string list) =
    let firstFewElements = words |> List.take n
    words @ firstFewElements

let getPhraseWordPairs (text : string) phraseLength =
    text
    |> splitText
    |> removeInvalidWords
    |> appendFirstWords phraseLength // insures that there always is a next word by appending the first few
    |> Seq.windowed (phraseLength+1)
    |> Seq.map (splitOffLastWord >> concatPhrase)

let getPhrases (phraseWordPairs : seq<string*string>) =
    phraseWordPairs
    |> Seq.map (fun (phrase,_) -> phrase)

let updateMarkovMap (map : Map<string,string list>) (phrase, nextWord) =
    if map.ContainsKey phrase then
        let previousValue = map.[phrase]
        map
        |> Map.remove phrase
        |> Map.add phrase (nextWord :: previousValue)
    else
        map |> Map.add phrase [nextWord]

let getMarkovMap (phraseWordPairs : seq<string * string>) =
    phraseWordPairs
    |> Seq.fold updateMarkovMap Map.empty

// ===============================================================================================

let rnd = Random()

let getRandomItem seq =
    let length = seq |> Seq.length
    seq |> Seq.item (rnd.Next length)

let getPreviousWords phraseLength words =
    words
    |> Seq.windowed phraseLength
    |> Seq.last

let getNextWord (map : Map<string,string list>) (previousWords : string[]) =
    previousWords
    |> joinWords
    |> (fun phrase -> map.Item phrase)
    |> getRandomItem

let appendMostProbableWord map phraseLength (newText : string list) =
    newText
    |> getPreviousWords phraseLength
    |> getNextWord map
    |> fun w -> newText @ [w]

let generateText (markovMap : Map<string, string list>) startPhrase phraseLength amount =
    let mutable newtext = splitText startPhrase |> Seq.toList
    for i in 1..amount do
        newtext <- appendMostProbableWord markovMap phraseLength newtext
    newtext


let text = File.ReadAllText "./example-file.txt"

let phraseLength = 2
let phraseWordPairs = getPhraseWordPairs text phraseLength

let map = getMarkovMap phraseWordPairs

let startPhrase = getPhrases phraseWordPairs |> getRandomItem
                  

generateText map startPhrase phraseLength 100
|> joinWords
|> printfn "%A"