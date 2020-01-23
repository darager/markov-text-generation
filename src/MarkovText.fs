namespace MarkovText

open MarkovTextUtils

module MarkovText =

    type TextGenerator(text, phraseLength) =
        let pairs = MarkovTextUtils.getPhraseWordPairs text phraseLength
        let map = MarkovTextUtils.buildMarkovMap pairs

        member this.GenerateText length =
            let startPhrase = MarkovTextUtils.getPhrases pairs
                              |> MarkovTextUtils.getRandomItem

            MarkovTextUtils.appendGeneratedWords map startPhrase phraseLength (length-phraseLength)
            |> MarkovTextUtils.joinWords