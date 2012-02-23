import Data.Array
import Data.List
import Control.Monad
import Test.HUnit

import QRShared
import QREncode
import QRDraw

testBits = [False,False,True,False,False,False,False,False,False,True,False,True,True,False,True,True,False,False,False,False,True,False,True,True,False,True,True,True,True,False,False,False,True,True,False,True,False,False,False,True,False,True,True,True,False,False,True,False,True,True,False,True,True,True,False,False,False,True,False,False,True,True,False,True,False,True,False,False,False,False,True,True,False,True,False,False,False,False,False,False,True,True,True,False,True,True,False,False,False,False,False,True,False,False,False,True,False,False,False,False,False,False,False,False,True,False,True,False,True,False,False,False,False,True,False,False,True,False,False,False,False,False,False,True,False,True,True,False,False,True,False,True,False,False,True,False,True,True,False,True,True,False,False,True,False,False,True,True,False,True,True,False,True,False,False,True,True,True,False,False,False,False,False,False,False,False,False,False,False,False,True,False,True,True,True,False,False,False,False,False,True,True,True,True,True,False,True,True,False,True,False,False,False,True,True,True,True,False,True,False,False,False,False,True,False,False,False,False]
testImage = (dataToBitmap testBits Q Mask0)

tests = test [
    "scoreSequence" ~: test [
    scoreSequence [] ~?= 0,
    scoreSequence [True, True, True, True] ~?= 0,
    scoreSequence [True, True, True, True,True] ~?= 3,
    scoreSequence [True,True,True,True,True,True] ~?= 4,
    scoreSequence [False,False,False,False,False,False,False] ~?= 5,
    scoreSequence [False,False,False,True,False,False,False,False,False] ~?= 3
    ],
    "penaltyRules" ~: test [
    penaltyRule1 testImage ~?= 180,
    penaltyRule2 testImage ~?= 90,
    penaltyRule3 testImage ~?= 80,
    penaltyRule4 testImage ~?= 0,
    penalty      testImage ~?= 350,

    penalty (dataToBitmap testBits Q Mask1) ~?= 421,
    penalty (dataToBitmap testBits Q Mask2) ~?= 507,
    penalty (dataToBitmap testBits Q Mask3) ~?= 443,
    penalty (dataToBitmap testBits Q Mask4) ~?= 533,
    penalty (dataToBitmap testBits Q Mask5) ~?= 547,
    penalty (dataToBitmap testBits Q Mask6) ~?= 357,
    penaltyRule1 (dataToBitmap testBits Q Mask7) ~?= 197,
    penaltyRule2 (dataToBitmap testBits Q Mask7) ~?= 123,
    penaltyRule3 (dataToBitmap testBits Q Mask7) ~?= 200,
    penaltyRule4 (dataToBitmap testBits Q Mask7) ~?= 0,
    penalty (dataToBitmap testBits Q Mask7) ~?= 520
    ],
    "addAlignmentPattern" ~: test [
    (addAlignmentPattern 0 0 (blankImage 8 8)) ! (4,4) ~?= True,
    (addAlignmentPattern 1 1 (blankImage 8 8)) ! (0,0) ~?= False
    ],
    "blankImage" ~: test [
    (blankImage 1 1) ! (0,0) ~?= False,
    indices (blankImage 2 2) ~?= [(0,0),(0,1),(1,0),(1,1)]
    ],
    "encodeString" ~: test [
    humanReadableBinary (encodeString 104 "HELLO WORLD") ~?= "00100000010110110000101101111000110100010111001011011100010011010100001101000000111011000001000111101100"
    ],
    "stringWithErrorCorrection" ~: test [
    humanReadableBinary (stringWithErrorCorrection 1 Q "HELLO WORLD") ~?= "0010000001011011000010110111100011010001011100101101110001001101010000110100000011101100000100011110110010101000010010000001011001010010110110010011011010011100000000000010111000001111101101000111101000010000"
    ],
    "multiplyPolynomial" ~: test [
    let
        m = messagePolynomial 13 [32, 91, 11, 120, 209, 114, 220, 77, 67, 64, 236, 17, 236]
        g = generatorPolynomial 13
    in multiplyPolynomial m g ~?= [(12,168),(11,72),(10,22),(9,82),(8,217),(7,54),(6,156),(5,0),(4,46),(3,15),(2,180),(1,122),(0,16)]
    ],
    "polyToAlphaPoly" ~: test [
    polyToAlphaPoly [(25, 32),(24,91),(23,11)] ~?= [(25, 5),(24,92),(23,238)]
    ],
    "integerToExponent" ~: test [
    integerToExponent 11 ~?= 238
    ],
    "exponentToInteger" ~: test [
    exponentToInteger 14 ~?= 19
    ],
    "messagePolynomial" ~: test [
        test $ assertPrefixOf [(29, 32), (28, 91), (27, 11), (26, 120), (25, 209)] (messagePolynomial 17 [32, 91, 11, 120, 209, 114, 220, 77, 67, 64, 236, 17, 236])
    ],
    "dataBlocks" ~: test [
        dataBlocks [True, False, False, False, False, False, False, False] ~?= [128]
       ,dataBlocks [True, False, False, False, False, False, False, False, True, False, False, False, False, False, False, False] ~?= [128, 128]
        ],
    "binaryToInt" ~: test [
        binaryToInt [False] ~?= 0
       ,binaryToInt [True] ~?= 1
       ,binaryToInt [False, True, True, False, False, False] ~?= 24
       ,binaryToInt [True, True, True, True] ~?= 15
        ]
    ]

main = runTestTT tests

-- ASSERTIONS --

assertPrefixOf :: (Show a, Eq a) => [a] -> [a] -> Assertion
assertPrefixOf prefix actual =
    unless (prefix `isPrefixOf` actual) (assertFailure msg)
  where msg = "expected " ++ show prefix ++ " to be a prefix of " ++ show actual

-- UTILITIES --
humanReadableBinary :: [Bool] -> String
humanReadableBinary [] = []
humanReadableBinary (True:xs) = '1':humanReadableBinary xs
humanReadableBinary (False:xs) = '0':humanReadableBinary xs
