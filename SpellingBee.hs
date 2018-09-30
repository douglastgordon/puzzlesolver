-- To Run:
-- set centralLetter and ringLetters according to puzzle
-- $ runghc SpellingBee < dictionary.txt

-- dependent on puzzle
centralLetter = 'n'
ringLetters = "vadilu"

forbiddenLetters = filter (\letter -> letter `notElem` ringLetters && letter /= centralLetter) ['a'..'z']

lengthFiveOrMore :: [a] -> Bool
lengthFiveOrMore = (>=5) . length

usesCentralLetter :: [Char] -> Bool
usesCentralLetter = elem centralLetter

usesValidLettersOnly :: [Char] -> Bool
usesValidLettersOnly = foldl (\acc letter -> acc && (letter `notElem` forbiddenLetters)) True

conditions = [lengthFiveOrMore, usesCentralLetter, usesValidLettersOnly]

main = interact foundWords
    where foundWords input = show $ filter (\word -> lengthFiveOrMore word && usesCentralLetter word && usesValidLettersOnly word) (words input)
