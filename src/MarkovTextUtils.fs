namespace MarkovTextUtils

open System
open System.Text.RegularExpressions


module MarkovTextUtils =

    let joinWords = String.concat " "

    let splitText (text: string) =
        text.Split [|' '|]
        |> Array.toList

    let replaceSpecialCharacters text =
        Regex.Replace (text, @"\\\r", " ")

    let removeInvalidWords words =
        words
        |> List.map replaceSpecialCharacters
        |> List.filter (String.IsNullOrEmpty >> not)


    let concatPhrase (phrase, followingWord) =
        (joinWords phrase, followingWord)

    let splitOffLastWord (wordpair: string[]) =
        let length = wordpair.Length
        let precedingWords = wordpair |> Seq.take (length-1)
        (precedingWords, wordpair.[length-1])

    let appendFirstWords n words =
        let firstFewElements = words |> List.take n
        words @ firstFewElements

    let getPhraseWordPairs text phraseLength =
        text
        |> splitText
        |> removeInvalidWords
        |> appendFirstWords phraseLength // ensures that there always is a next word
        |> Seq.windowed (phraseLength+1)
        |> Seq.map (splitOffLastWord >> concatPhrase)


    let updateMarkovMap (map: Map<string,string list>) (phrase, nextWord) =
        if map.ContainsKey phrase then
            let previousValue = map.[phrase]
            map
            |> Map.remove phrase
            |> Map.add phrase (nextWord:: previousValue)
        else
            map |> Map.add phrase [nextWord]

    let buildMarkovMap (phraseWordPairs: seq<string * string>) =
        phraseWordPairs
        |> Seq.fold updateMarkovMap Map.empty


    let rnd = Random()
    let getRandomItem seq =
        let index = seq |> Seq.length |> rnd.Next
        seq |> Seq.item index

    let getPreviousWords phraseLength words =
        words
        |> Seq.windowed phraseLength
        |> Seq.last

    let getNextWord (map: Map<string,string list>) previousWords =
        previousWords
        |> joinWords
        |> fun phrase -> map.Item phrase
        |> getRandomItem

    let appendMostProbableWord map phraseLength newText =
        newText
        |> getPreviousWords phraseLength
        |> getNextWord map
        |> fun w -> newText @ [w]

    let getPhrases (phraseWordPairs: seq<string*string>) =
        phraseWordPairs
        |> Seq.map (fun (phrase,_) -> phrase)

    let appendGeneratedWords (markovMap: Map<string, string list>) startPhrase phraseLength amount =
        let mutable newtext = splitText startPhrase
        for _ in 1..amount do
            newtext <- appendMostProbableWord markovMap phraseLength newtext
        newtext