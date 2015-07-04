import Text.ParserCombinators.Parsec

token :: Parser
token =
    do
        {-identifier
        <|> boolean
        <|> number
        <|>-} character
        {-<|> string
        <|> char '('
        <|>-}

character :: Parser
character 
