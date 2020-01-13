namespace MarkovText

open MarkovTextUtils

module MarkovText =
    type TextGenerator(text, phraseLength) =

        member this.PhraseLength = phraseLength
        member this.Pairs = MarkovTextUtils.getPhraseWordPairs text phraseLength
        member this.Map = MarkovTextUtils.buildMarkovMap this.Pairs

        member this.GenerateText length =
            let startPhrase = MarkovTextUtils.getPhrases this.Pairs
                              |> MarkovTextUtils.getRandomItem

            MarkovTextUtils.appendGeneratedWords this.Map startPhrase this.PhraseLength (length-phraseLength)
            |> MarkovTextUtils.joinWords