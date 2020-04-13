import Text.ParserCombinators.Parsec

-- compared to csv1.hs, this one is much more simple and short.
csvFile = line `endBy` eol
line = cell `sepBy` (char ',')
cell = many (noneOf ",\n")
eol = char '\n'

parseCSV :: String -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input
