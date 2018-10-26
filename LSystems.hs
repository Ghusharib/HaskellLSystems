module LSystems where

import IC.Graphics

type Rule
  = (Char, String)

type Rules
  = [Rule]

type System
  = (Float, String, Rules)

cross, triangle, arrowHead, peanoGosper,
  dragon, snowflake, tree, bush :: System

type Vertex
  = (Float, Float)

type TurtleState
  = (Vertex, Float)

type Stack
  = [TurtleState]

type ColouredLine
  = (Vertex, Vertex, Colour)

--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --
--  Functions for working with systems.

-- |Returns the rotation angle for the given system.
angle :: System -> Float
angle (returnValue, _, _)
  = returnValue

-- |Returns the base string for the given system.
base :: System -> String
base (_, returnValue, _)
  = returnValue

-- |Returns the set of rules for the given system.
rules :: System -> Rules
rules (_, _, returnValue)
  = returnValue

-- |Look up a character in the given set of rules.
--
--  Pre: the character exists in the set of rules.
lookupChar :: Char -> Rules -> String
lookupChar _ []
  = error "This character does not exist in the rules"
lookupChar toFind rules
  = head [y | (x, y) <- rules, x == toFind]

-- |Expand a command once using the given set of rules.
expandOne :: Rules -> String -> String
expandOne _ []
  = []
expandOne givenRules (x : xs)
  = lookupChar x givenRules ++ expandOne givenRules xs

-- |Expand a command `n' times using the given set of rules.
expand :: Rules -> String -> Int -> String
expand _ finalValue 0
  = finalValue
expand givenRules givenString n
  = expand givenRules expansionResult (n-1)
  where
    expansionResult = expandOne givenRules givenString

-- |Move a turtle.
--
--  * 'F' moves distance 1 in the current direction.
--  * 'L' rotates left according to the given angle.
--  * 'R' rotates right according to the given angle.
move :: Char -> TurtleState -> Float -> TurtleState
move comparisonChar currentState rotationAngle
  |comparisonChar == 'F' = ((newx, newy), currentAngle)
  |comparisonChar == 'R' = ((x, y), (currentAngle - rotationAngle))
  |comparisonChar == 'L' = ((x, y), (currentAngle + rotationAngle))
  where
    newx                   = x + (cos (currentAngle * conversion))
    newy                   = y + (sin (currentAngle * conversion))
    ((x, y), currentAngle) = currentState
    conversion             = pi/180
    --conversion is multiplying to convert to radians as cos works in radians

-- |Trace lines drawn by a turtle using the given colour, following the
--  commands in the string and assuming the given initial angle of rotation.
--  Method 1

trace1 :: String -> Float -> Colour -> [ColouredLine]
trace1 rules angle colour
  = snd (trace1' rules ((0, 0), 90))
  where
    trace1' :: String -> TurtleState -> (String, [ColouredLine])
    trace1' [] _
      = ("", [])
    trace1' (rule : rules) currentState@((x, y), _)
      | rule == '[' = (inBraceLeft, inBraceAnswer ++ afterBraceAnswer)
      | rule == ']' = (rules, [])
      | rule == 'F' = (leftOver, ((x, y), (newx, newy), colour) : answer)
      | otherwise   = (leftOver, answer)
      where
        (leftOver, answer)           = trace1' rules newState
        (inBraceLeft, inBraceAnswer) = trace1' rules currentState
        (_, afterBraceAnswer)        = trace1' inBraceLeft currentState
        newState@((newx, newy), _)   = move rule currentState angle

-- |Trace lines drawn by a turtle using the given colour, following the
--  commands in the string and assuming the given initial angle of rotation.
--  Method 2
trace2 :: String -> Float -> Colour -> [ColouredLine]
trace2 rules angle colour
  = trace2' rules ((0, 0), 90) []
  where
    trace2' :: String -> TurtleState -> Stack -> [ColouredLine]
    trace2' [] _ _
      = []
    trace2' ('[' : rules) currentState stack
      = trace2' rules currentState (currentState : stack)
    trace2' (']' : rules) currentState (stack : stacks)
      = trace2' rules stack stacks
    trace2' (rule : rules) currentState@((x,y),_) stack
      |rule == 'F' = ((x, y), (newx, newy), colour) : trace2' rules newState stack
      |otherwise   = trace2' rules newState stack
      where
        newState@((newx, newy), _) = move rule currentState angle

--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

--  Some test systems.

cross
  = ( 90
    , "M-M-M-M"
    , [ ('M', "M-M+M+MM-M-M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

triangle
  = ( 90
    , "-M"
    , [ ('M', "M+M-M-M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

arrowHead
  = ( 60
    , "N"
    , [ ('M', "N+M+N")
      , ('N', "M-N-M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

peanoGosper
  = ( 60
    , "M"
    , [ ('M', "M+N++N-M--MM-N+")
      , ('N', "-M+NN++N+M--M-N")
      , ('+', "+")
      , ('-', "-")
      ]
    )

dragon
  = ( 45
    , "MX"
    , [ ('M', "A")
      , ('X', "+MX--MY+")
      , ('Y', "-MX++MY-")
      , ('A', "A")
      , ('+', "+")
      , ('-', "-")
      ]
    )

snowflake
  = ( 60
    , "M--M--M"
    , [ ('M', "M+M--M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

tree
  = ( 45
    , "M"
    , [ ('M', "N[-M][+M][NM]")
      , ('N', "NN")
      , ('[', "[")
      , (']', "]")
      , ('+', "+")
      , ('-', "-")
      ]
    )

bush
  = ( 22.5
    , "X"
    , [ ('X', "M-[[X]+X]+M[+MX]-X")
      , ('M', "MM")
      , ('[', "[")
      , (']', "]")
      , ('+', "+")
      , ('-', "-")
      ]
    )

mapper :: Rules
mapper
  = [ ('M', "F")
    , ('N', "F")
    , ('X', "")
    , ('Y', "")
    , ('A', "")
    , ('[', "[")
    , (']', "]")
    , ('+', "L")
    , ('-', "R")
    ]

lSystem :: System -> Int -> String
lSystem (_, base, rs) n
  = expandOne mapper (expand rs base n)

drawLSystem1 :: System -> Int -> Colour -> IO ()
drawLSystem1 system n colour
  = drawLines (trace1 (lSystem system n) (angle system) colour)

drawLSystem2 :: System -> Int -> Colour -> IO ()
drawLSystem2 system n colour
  = drawLines (trace2 (lSystem system n) (angle system) colour)
