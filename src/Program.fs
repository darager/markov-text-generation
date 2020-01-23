open System.IO
open MarkovText

let text = File.ReadAllText "./example-file.txt"
let textGenerator = MarkovText.TextGenerator (text, 3)

textGenerator.GenerateText 10
|> printfn "%A"