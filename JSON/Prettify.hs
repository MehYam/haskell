import SimpleJSON

data Doc = ToBeDefined deriving (Show)

string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

text :: String -> Doc
text s = undefined

double :: Double -> Doc
double d = undefined


enclose :: Char -> Char -> Doc -> Doc
enclose left right doc = char left <> doc <> char right

(<>) :: Doc -> Doc -> Doc
a <> b = undefined

char :: Char -> Doc
char c = undefined

hcat :: [Doc] -> Doc
hcat docs = undefined

oneChar :: Char -> Doc
oneChar c = case lookup c charEscapes of
	          Just r -> text r
	          Nothing | mustEscape c -> hexEscape c
	                  | otherwise -> char c
	        where  mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

charEscapes :: [(Char, String)]
charEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
	where ch a b = (a, ['\\', b])
