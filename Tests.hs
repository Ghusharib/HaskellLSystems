module Tests where

import IC.TestSuite
import IC.Graphics
import LSystems

import Data.List (sort)

angleTestCases
  = [ cross       ==> 90
    , (1, "", []) ==> 1
    , triangle    ==> 90
    , arrowHead   ==> 60
    ]

baseTestCases
  = [ (0, "+", []) ==> "+"
    , cross        ==> "M-M-M-M"
    , triangle     ==> "-M"
    , arrowHead    ==> "N"
    ]

rulesTestCases
  = [ cross ==> [ ('M', "M-M+M+MM-M-M+M")
                , ('+', "+")
                , ('-', "-")
                ]
    , (0, "", [ ('M', "N") ])
        ==> [ ('M', "N") ]
    ]

{- Note: these test cases use angle/base/rules, and will fail the test
 - suite with Argument exceptions until those functions are correctly
 - implemented.
 -}
lookupCharTestCases
  = [ ('X', [ ('X', "Yes")
            , ('Y', "No")])  ==> "Yes"
    , ('X', [ ('Y', "No")
            , ('X', "Yes")]) ==> "Yes"
    , ('M',  (rules peanoGosper))
      ==> "M+N++N-M--MM-N+"
    , ('+', (rules triangle))
      ==> "+"
    ]

expandOneTestCases
  = [ (rules triangle, base triangle)
        ==> "-M+M-M-M+M"
    , ([('A', "B")], "A") ==> "B"
    ]

expandTestCases
  = [ (rules arrowHead, base arrowHead, 2)
        ==> "N+M+N-M-N-M-N+M+N"

    , (rules dragon, base dragon, 0)
        ==> "MX"

    , (rules dragon, base dragon, 1)
        ==> "A+MX--MY+"

    , (rules dragon, base dragon, 5)
        ==> concat [ "A+A+A+A+A+MX--MY+--A-MX++MY-+--A-A+MX--MY+++A-MX++MY--+"
                   , "--A-A+A+MX--MY+--A-MX++MY-+++A-A+MX--MY+++A-MX++MY---+-"
                   , "-A-A+A+A+MX--MY+--A-MX++MY-+--A-A+MX--MY+++A-MX++MY--++"
                   , "+A-A+A+MX--MY+--A-MX++MY-+++A-A+MX--MY+++A-MX++MY----+"
                   ]
    ]

moveTestCases
  = [ ('L', ((100, 100), 90), 90) ==> ((100.0,100.0),180.0)
    , ('F', ((50,50), 60), 60)    ==> ((50.5,50.866024),60.0)
    , ('F', ((-25,180),180), 45)  ==> ((-26.0,180.0),180.0)
    --Added in test case for R as well as it was currently untested
    --Also for those outside range 0 - 360
    , ('R', ((0, 69), 75), 60)    ==> ((0.0, 69.0), 15.0)
    , ('L', ((100, 100), 290), 90) ==> ((100.0,100.0), 380.0)
    , ('R', ((100, 100), 0), 135) ==> ((100.0,100.0), -135.0)
    ]

traceTestCases
  = [ ((expandOne mapper (expand (rules triangle)
            (base triangle) 1)), (angle triangle), blue)
      ==> sort [ ((0.0,0.0),(1.0,0.0),(0.0,0.0,1.0))
               , ((1.0,0.0),(0.99999994,1.0),(0.0,0.0,1.0))
               , ((0.99999994,1.0),(2.0,1.0),(0.0,0.0,1.0))
               , ((2.0,1.0),(2.0,0.0),(0.0,0.0,1.0))
               , ((2.0,0.0),(3.0,0.0),(0.0,0.0,1.0))
               ],
      ((expandOne mapper (expand (rules tree)
            (base tree) 1)), (angle tree), red)
      ==> sort [ ((0.0,0.0),(-4.371139e-8,1.0),(1.0,0.0,0.0))
                ,((-4.371139e-8,1.0),(0.7071067,1.7071068),(1.0,0.0,0.0))
                ,((-4.371139e-8,1.0),(-0.7071068,1.7071068),(1.0,0.0,0.0))
                ,((-4.371139e-8,1.0),(-8.742278e-8,2.0),(1.0,0.0,0.0))
                ,((-8.742278e-8,2.0),(-1.3113416e-7,3.0),(1.0,0.0,0.0))]
    ]


allTestCases
  = [ TestCase "angle"      (angle . unId)
                            (map mkId angleTestCases)
    , TestCase "base"       (base . unId)
                            (map mkId baseTestCases)
    , TestCase "rules"      (rules . unId)
                            (map mkId rulesTestCases)
    , TestCase "lookupChar" (uncurry lookupChar)
                            lookupCharTestCases
    , TestCase "expandOne"  (uncurry expandOne)
                            expandOneTestCases
    , TestCase "expand"     (uncurry3 expand)
                            expandTestCases
    , TestCase "move"       (uncurry3 move)
                            moveTestCases
    , TestCase "trace1"     (sort . (uncurry3 trace1))
                            traceTestCases
    , TestCase "trace2"     (sort . (uncurry3 trace2))
                            traceTestCases
    ]

runTests = mapM_ goTest allTestCases

main = runTests
