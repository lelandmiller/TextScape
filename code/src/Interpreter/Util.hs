module Interpreter.Util where




{- | The 'splitString' function takes a 'String', and a delimiter
     'Char', and  returns a '[String]' of the words from left to right
     separated by the delimiter.
-}
splitString :: String -> Char -> [String]
splitString "" _ = []
splitString string delim =
         front : splitString backt delim
         where front = takeWhile (/= delim) string
               back  = dropWhile (/= delim) string
               backt = case back of
                        "" -> ""
                        _  -> tail back

