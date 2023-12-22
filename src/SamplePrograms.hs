{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module SamplePrograms (main, treeDemoA) where

import Interpreter (runProgram)
import Text.RawString.QQ (r)
import Text.Megaparsec ( errorBundlePretty )
import Parser.Parser (parseBasicProgram')


treeDemoA :: String
treeDemoA = unlines 
  [
    "10 PRINT",
    "20 FOR i = 1 TO 12",
    "25 REM place the star",
    "30 IF i = 1 THEN",
    "40 COLOR \"Yellow\"",
    "45 FOR s = 1 TO 11",
    "46 PRINT \" \";",
    "47 NEXT s",
    "48 PRINT \"*\"",
    "60 END",
    "65 REM draw the tree body",
    "70 LET spaces = 12 - i",
    "80 LET tree = i * 2 - 1",
    "90 FOR j = 1 TO spaces",
    "110 PRINT \" \";",
    "120 NEXT j",
    "130 FOR k = 1 TO tree",
    "140 REM place decorations or tree body",
--
    "150 IF i = 2 AND k = 1 THEN",
    "160 COLOR \"Red\"",
    "170 PRINT \"O\";",
    "180 ELSE",
    "181 IF i = 5 AND k = 7 THEN",  -- First additional condition
    "182 COLOR \"Magenta\"",
    "183 PRINT \"O\";",
    "184 ELSE",
    "185 IF i = 7 AND k = 6 THEN",  -- Second additional condition
    "186 COLOR \"Red\"",
    "187 PRINT \"O\";",
    "188 ELSE",
    "189 IF i = 9 AND k = 12 THEN",  -- Third additional condition (original condition repeated)
    "190 COLOR \"Cyan\"",
    "191 PRINT \"O\";",
    "192 ELSE",
    "193 IF i = 11 AND k = 9 THEN",  -- Fourth additional condition
    "194 COLOR \"Yellow\"",
    "195 PRINT \"O\";",
    "196 ELSE",
    "197 COLOR \"Green\"",
    "198 PRINT \"/\";",
    "199 END",
    "200 END",
    "201 END",
    "202 END",
    "203 END",
    "204 REM end of decorations or tree body",
--
    "220 NEXT k",
    "230 PRINT",
    "240 NEXT i",
    "250 COLOR \"Brown\"",
    "260 PRINT \"         ||||\"",
    "270 COLOR \"Red\"",
    "280 PRINT \"       ________\""
  ]



treeDemoA'' :: String
treeDemoA'' =
  [r|
10 PRINT
20 FOR i = 1 TO 12
25 REM place the star
30 IF i = 1 THEN
40 COLOR "Yellow"
45 FOR s = 1 TO 11
46 PRINT " ";
47 NEXT s
48 PRINT "*"
60 END
65 REM draw the tree body
70 LET spaces = 12 - i
80 LET tree = i * 2 - 1
90 FOR j = 1 TO spaces
110 PRINT " ";
120 NEXT j
130 FOR k = 1 TO tree
140 REM place decorations or tree body
150 IF i = 2 AND k = 1 THEN
160 COLOR "Red"
170 PRINT "O";
180 ELSE
181 IF i = 5 AND k = 1 THEN
182 COLOR "Red"
183 PRINT "O";
184 END
190 COLOR "Green"
200 PRINT "/";
210 END
220 NEXT k
230 PRINT
240 NEXT i
250 COLOR "Brown"
260 PRINT "         ||||"
270 COLOR "Red"
280 PRINT "       ________"
|]


treeDemoA' :: String
treeDemoA' =
  [r|
5 PRINT
10 FOR i = 1 TO 12
15 IF i = 1 THEN
16 COLOR "Yellow"
17 PRINT "           *"
18 END
20 LET spaces = 12 - i
30 LET tree = i * 2 - 1
40 FOR j = 1 TO spaces
45 COLOR "Green"
50 PRINT " ";
60 NEXT j
70 FOR k = 1 TO tree
80 PRINT "/";
90 NEXT k
100 PRINT
110 NEXT i
125 COLOR "Brown"
120 PRINT "         ||||"
125 COLOR "Red"
130 PRINT "       ________"
|]


treeDemoB :: String
treeDemoB =
  [r|
5 PRINT "."
10 FOR i = 1 TO 8
20 LET spaces = 8 - i
30 LET stars = i * 2 - 1
40 FOR j = 1 TO spaces
50 PRINT " ";
60 NEXT j
70 FOR k = 1 TO stars
80 PRINT "/";
90 NEXT k
100 PRINT
110 NEXT i
120 PRINT "      ||||"
130 PRINT "    ________"
|]


longerTestProg :: String
longerTestProg =
  [r|
5 REM test
10 LET z = 6
20 FOR i = 1 TO 10
30 PRINT i
40 IF i = z THEN
50 PRINT "Boom!"
60 END
70 NEXT i
80 LET x = z + 1
90 PRINT "Expecting 7"
100 PRINT x
110 PRINT 5 - 2
120 LET a = -1
130 PRINT a
135 PRINT "Should print 6 >"
140 PRINT 7 + a
145 PRINT "Should print 8 >"
150 PRINT 2 * 4
155 PRINT "Should print 3 >"
160 PRINT 9 / 3
|]


ifElseTestProg :: String
ifElseTestProg =
  [r|
10 REM test
20 FOR i = 1 TO 10
25 PRINT i
30 IF i = 5 THEN
40 PRINT "Five"
50 ELSE
60 PRINT "Not five"
80 END
90 NEXT i
|]

ifElseAndTestProg :: String
ifElseAndTestProg =
  [r|
10 REM test
20 FOR i = 1 TO 3
24 FOR o = 2 TO 5
25 PRINT i
26 PRINT o
30 IF i = 2 AND o = 5 THEN
40 PRINT "2 AND 5"
50 ELSE
60 PRINT "other"
80 END
85 NEXT o
90 NEXT i
|]


mainEProgram :: String
mainEProgram = [r|
10 LET z = 7
20 FOR i = 1 TO 10
35 PRINT i
40 IF i = z THEN
50 PRINT "Boom!"
60 END
70 NEXT i
|]




main :: IO ()
main = do
  -- let input = longerTestProg
  let input = treeDemoA
  -- let input = ifElseTestProg
  --let input = ifElseAndTestProg
  putStrLn "Input program\n"
  putStrLn input
  putStrLn ""
  case parseBasicProgram' input of
    Left bundle -> putStr (errorBundlePretty bundle) -- Print parsing errors
    Right ast -> do
        putStrLn "Abstract syntax tree\n"
        print ast -- Print the abstract syntax tree (AST)
        _ <- runProgram ast
        return ()