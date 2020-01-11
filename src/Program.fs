open System
open System.IO

let splitText (text : string) =
    text.Split [|' '|]

let removeLinebreak (text : string) =
    text.Replace ("\n", " ")

let removeInvalidWords words =
    words
    |> Seq.map removeLinebreak
    |> Seq.filter (String.IsNullOrEmpty >> not)

let joinWords = String.concat " "

let splitOffLastWord (wordpair : string[]) =
    let length = wordpair.Length
    let precedingWords = wordpair |> Seq.take (length-1)
    (precedingWords, wordpair.[length-1])

let concatPhrase (phrase, followingWord) =
    (joinWords phrase, followingWord)

let getPhraseWordPairs text phraseLength =
    text
    |> splitText
    |> removeInvalidWords
    |> Seq.windowed (phraseLength+1)
    |> Seq.map (splitOffLastWord >> concatPhrase)

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


let text = File.ReadAllText "./example-file.txt"

let phraseLength = 3
let phraseWordPairs = getPhraseWordPairs text phraseLength

let map = getMarkovMap phraseWordPairs

printfn "%A" map











// let rnd = Random()
// let phrases = phraseWordPairs |> Seq.map (fun (_,word) -> word)


// let getRandomItem seq =
//     let length = Seq.length seq
//     seq |> Seq.item (rnd.Next length)

// let getPreviousWords phraseLength words =
//     words
//     |> Seq.windowed phraseLength
//     |> Seq.last

// let getNextWord (map : Map<_,_>) (previousWords : string[]) =
//     previousWords
//     |> joinWords
//     |> map.TryGetValue
//     |> fun (_,v) -> getRandomItem v

// let buildNewText pairlength map newText =
//     newText
//     |> getPreviousWords (pairlength-1)
//     |> getNextWord map
//     |> fun w -> newText @ [w]

// let appendNewWords (markovMap : Map<string, string list>) startPhrase pairlength amount =
//     let mutable newtext = startPhrase
//     for i in 1..amount do
//         newtext <- buildNewText pairlength markovMap newtext
//     newtext

// let startPhrase = getRandomItem phrases
// let newText = splitText startPhrase |> Array.toList

// appendNewWords map newText phraseLength 40
// |> joinWords
// |> printfn "%A"