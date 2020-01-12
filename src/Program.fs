open System
open System.Text.RegularExpressions


let splitText (text : string) =
    text.Split [|' '|]

let removeSpecialCharacters (text : string) =
    Regex.Replace (text, @"\\\r", " ")

let removeInvalidWords words =
    words
    |> Seq.map removeSpecialCharacters
    |> Seq.filter (String.IsNullOrEmpty >> not)

let joinWords = String.concat " "

let splitOffLastWord (wordpair : string[]) =
    let length = wordpair.Length
    let precedingWords = wordpair |> Seq.take (length-1)
    (precedingWords, wordpair.[length-1])

let concatPhrase (phrase, followingWord) =
    (joinWords phrase, followingWord)

let getPhraseWordPairs (text : string) phraseLength =
    text
    |> splitText
    |> removeInvalidWords
    |> fun w -> w |> Seq.append (w |> Seq.take (phraseLength+1)) // make sure there always is a following word for every phrase
    |> Seq.windowed (phraseLength+1)
    |> Seq.map (splitOffLastWord >> concatPhrase)
    |> fun pairs -> pairs |> Seq.take ((pairs |> Seq.length)-1) // remove duplicate word (duplicate phrase)
    // TODO: a problem occurs when the text generation approaches the end of the sample text

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

//===============================================================================================

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


// let text = File.ReadAllText "./example-file.txt"
let text = "F# is a functional programming language that makes it easy to write correct and maintainable code.
        F# programming primarily involves defining types and functions that are type-inferred and generalized automatically. This allows your focus to remain on the problem domain and manipulating its data, rather than the details of programming.
        F# has full support for objects, which are useful data types when you need to blend data and functionality. F# functions are used to manipulate objects.
        F# functions are also first-class, meaning they can be passed as parameters and returned from other functions.
        Rather than writing code that is object-oriented, in F#, you will often write code that treats objects as another data type for functions to manipulate. Features such as generic interfaces, object expressions, and judicious use of members are common in larger F# programs."

let phraseLength = 2
let phraseWordPairs = getPhraseWordPairs text phraseLength
let phrases = getPhrases phraseWordPairs
let startPhrase = getRandomItem phrases

let map = getMarkovMap phraseWordPairs

let generatedText = generateText map startPhrase phraseLength 100

generatedText
|> joinWords
|> printfn "%A"