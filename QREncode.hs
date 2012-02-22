module QREncode
( ECCLevel(L, M, Q, H)
, numToBinary
, encodePairs
, encodeString
, asciiValue
, dataBlocks
, binaryToInt
, messagePolynomial
, integerToExponent
, exponentToInteger
, polyToAlphaPoly
, alphaPolyToPoly
, generatorPolynomial
, multiplyPolynomial
, stringWithErrorCorrection
) where

import Char
import Data.List
import Data.Bits
import Debug.Trace

data ECCLevel = L | M | Q | H

numToBinary :: Int -> Int -> [Bool]
numToBinary 0 _ = []
numToBinary bits number
    | (div number (2^(bits-1)) == 1) = True : (numToBinary (bits-1) (mod number (2^(bits-1))))
    | otherwise = False : (numToBinary (bits-1) number)

encodePairs :: String -> [Bool]
encodePairs [] = []
encodePairs (a:[]) = numToBinary 6 (asciiValue a)
encodePairs (a:b:xs) =
    let a' = asciiValue a
        b' = asciiValue b
    in numToBinary 11 (a' * 45 + b') ++ encodePairs xs

encodeString :: Int -> String -> [Bool]
encodeString width str =
    let len              = numToBinary 9 (length str)
        mode             = [False, False, True, False]
        text             = encodePairs str
        dataLength       = (length text) + 4 + 9
        shortPadding     = take 4 (take (width - dataLength) (repeat False))
        extraBitsToEight = 8 - mod (dataLength + (length shortPadding)) 8
        eightBitPadding  = if extraBitsToEight == 0
                               then []
                               else take extraBitsToEight (repeat False)
        padBytes         = cycle [True, True, True, False, True, True, False, False, False, False, False, True, False, False, False, True]
        remainingPadLen  = width - (dataLength + (length shortPadding) + extraBitsToEight)
    in mode ++ len ++ text ++ shortPadding ++ eightBitPadding ++ (take remainingPadLen padBytes)

eccBlocks :: Int -> ECCLevel -> Int
eccBlocks 1 Q = 13
eccBlocks 1 H = 17

dataBytes :: Int -> ECCLevel -> Int
dataBytes 1 Q = 13
dataBytes 1 H = 9

stringWithErrorCorrection :: Int -> ECCLevel -> String -> [Bool]
stringWithErrorCorrection version ecclevel str =
    let width         = 8 * dataBytes version ecclevel
        encodedString = encodeString width str
        message       = dataBlocks encodedString
        eccBlocks'    = eccBlocks version ecclevel
        eccwords      = coefficients (multiplyPolynomial (messagePolynomial eccBlocks' message) (generatorPolynomial eccBlocks'))
    in encodedString ++ foldl (\a word -> a ++ (numToBinary 8 word)) [] eccwords

asciiValue :: Char -> Int
asciiValue ' ' = 36
asciiValue '$' = 37
asciiValue '%' = 38
asciiValue '*' = 39
asciiValue '+' = 40
asciiValue '-' = 41
asciiValue '.' = 42
asciiValue '/' = 43
asciiValue ':' = 44
asciiValue chr
    | isDigit chr = ord chr - 48
    | isUpper chr = ord chr - 65 + 10

dataBlocks :: [Bool] -> [Int]
dataBlocks binstr
    | length binstr <= 8 = [binaryToInt binstr]
    | otherwise          = binaryToInt (take 8 binstr) : dataBlocks (drop 8 binstr)

binaryToInt :: [Bool] -> Int
binaryToInt (True:[]) = 1
binaryToInt (False:[]) = 0
binaryToInt (True:xs) = 2^(length xs) + binaryToInt xs
binaryToInt (False:xs) = binaryToInt xs

-- (exponent, coefficient)
type Poly = [(Int, Int)]

type AlphaPoly = Poly

messagePolynomial :: Int -> [Int] -> Poly
messagePolynomial numCodeWords message =
    zip (reverse [1..firstExponent]) message
    where
        numBlocks = length message
        firstExponent = numBlocks + numCodeWords - 1

polyToAlphaPoly :: Poly -> AlphaPoly
polyToAlphaPoly [] = []
polyToAlphaPoly ((exponent, coefficient):xs) = (exponent, (integerToExponent coefficient)) : polyToAlphaPoly xs

alphaPolyToPoly :: AlphaPoly -> Poly
alphaPolyToPoly [] = []
alphaPolyToPoly ((exponent, coefficient):xs) = (exponent, (exponentToInteger coefficient)) : alphaPolyToPoly xs

generatorPolynomial :: Int -> AlphaPoly
generatorPolynomial 17 = [(17, 0), (16, 43), (15, 139), (14, 206), (13, 78), (12, 43), (11, 239), (10, 123), (9, 206), (8, 214), (7, 147), (6, 24), (5, 99), (4, 150)]
generatorPolynomial 13 = [(13, 0), (12, 74), (11, 152), (10, 176), (9, 100), (8, 86), (7, 100), (6, 106), (5, 104), (4, 130), (3, 218), (2, 206), (1, 140), (0, 78)]
generatorPolynomial 7  = [(7, 0), (6, 87), (5, 229), (4, 146), (3, 149), (2, 238), (1, 102), (0, 21)]

integerToExponent :: Int -> Int
integerToExponent integer = case find (\(exp, int) -> int == integer) logTable of
    Just (exp, int) -> exp
exponentToInteger exponent = case find (\(exp, int) -> exp == exponent) logTable of
    Just (exp, int) -> int

logTable = [(0,1),(1,2),(2,4),(3,8),(4,16),(5,32),(6,64),(7,128),(8,29),(9,58),(10,116),(11,232),(12,205),(13,135),(14,19),(15,38),(16,76),(17,152),(18,45),(19,90),(20,180),(21,117),(22,234),(23,201),(24,143),(25,3),(26,6),(27,12),(28,24),(29,48),(30,96),(31,192),(32,157),(33,39),(34,78),(35,156),(36,37),(37,74),(38,148),(39,53),(40,106),(41,212),(42,181),(43,119),(44,238),(45,193),(46,159),(47,35),(48,70),(49,140),(50,5),(51,10),(52,20),(53,40),(54,80),(55,160),(56,93),(57,186),(58,105),(59,210),(60,185),(61,111),(62,222),(63,161),(64,95),(65,190),(66,97),(67,194),(68,153),(69,47),(70,94),(71,188),(72,101),(73,202),(74,137),(75,15),(76,30),(77,60),(78,120),(79,240),(80,253),(81,231),(82,211),(83,187),(84,107),(85,214),(86,177),(87,127),(88,254),(89,225),(90,223),(91,163),(92,91),(93,182),(94,113),(95,226),(96,217),(97,175),(98,67),(99,134),(100,17),(101,34),(102,68),(103,136),(104,13),(105,26),(106,52),(107,104),(108,208),(109,189),(110,103),(111,206),(112,129),(113,31),(114,62),(115,124),(116,248),(117,237),(118,199),(119,147),(120,59),(121,118),(122,236),(123,197),(124,151),(125,51),(126,102),(127,204),(128,133),(129,23),(130,46),(131,92),(132,184),(133,109),(134,218),(135,169),(136,79),(137,158),(138,33),(139,66),(140,132),(141,21),(142,42),(143,84),(144,168),(145,77),(146,154),(147,41),(148,82),(149,164),(150,85),(151,170),(152,73),(153,146),(154,57),(155,114),(156,228),(157,213),(158,183),(159,115),(160,230),(161,209),(162,191),(163,99),(164,198),(165,145),(166,63),(167,126),(168,252),(169,229),(170,215),(171,179),(172,123),(173,246),(174,241),(175,255),(176,227),(177,219),(178,171),(179,75),(180,150),(181,49),(182,98),(183,196),(184,149),(185,55),(186,110),(187,220),(188,165),(189,87),(190,174),(191,65),(192,130),(193,25),(194,50),(195,100),(196,200),(197,141),(198,7),(199,14),(200,28),(201,56),(202,112),(203,224),(204,221),(205,167),(206,83),(207,166),(208,81),(209,162),(210,89),(211,178),(212,121),(213,242),(214,249),(215,239),(216,195),(217,155),(218,43),(219,86),(220,172),(221,69),(222,138),(223,9),(224,18),(225,36),(226,72),(227,144),(228,61),(229,122),(230,244),(231,245),(232,247),(233,243),(234,251),(235,235),(236,203),(237,139),(238,11),(239,22),(240,44),(241,88),(242,176),(243,125),(244,250),(245,233),(246,207),(247,131),(248,27),(249,54),(250,108),(251,216),(252,173),(253,71),(254,142),(255,1)]

padPolynomial :: Poly -> Poly
padPolynomial poly
    | smallestExponent == 0 = poly
    | otherwise             = padPolynomial (poly ++ [(smallestExponent-1, 0)])
    where smallestExponent = fst (last poly)

multiplyPolynomial :: Poly -> AlphaPoly -> Poly
multiplyPolynomial message generator
    | fst (last message) == 0 = message
    | otherwise =
        let alphaMessage     = polyToAlphaPoly message
            firstCoefficient = snd (head alphaMessage)
            generatorBump    = fst (head message) - fst (head generator)
            bumpedGenerator  = map (\(exponent, coefficient) -> (exponent+generatorBump, coefficient)) generator
            multiple         = map (\(exponent, coefficient) -> (exponent, mod (coefficient+firstCoefficient) 255)) bumpedGenerator
            resultIntegerTerms = alphaPolyToPoly multiple
            xorResult        = zipWith (\(e1,c1) (e2,c2) -> (e1, xor c1 c2)) (padPolynomial message) (resultIntegerTerms)
            strippedResult   = dropWhile (\(e,c) -> c == 0) xorResult
        in multiplyPolynomial strippedResult generator

coefficients :: Poly -> [Int]
coefficients poly = map snd poly
