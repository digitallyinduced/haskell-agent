-- | Animated lambda artwork used by the empty conversation view.
module Agent.CLI.TUI.LambdaArt
    ( lambdaArtWidget
    ) where

import Agent.CLI.TUI.Types (Name)
import Agent.TUI.Motion (shineOpacity)
import qualified Agent.TUI.Theme as Theme
import Brick (Widget)
import qualified Brick.Types as B
import Data.List (find)
import qualified Graphics.Vty as V

data LambdaComposition = LambdaComposition
    { lambdaUpperHeight :: !Int
    , lambdaLowerHeight :: !Int
    , lambdaStrokeWidth :: !Int
    , lambdaMarginX :: !Int
    , lambdaMarginY :: !Int
    , lambdaHasOrbit :: !Bool
    }

data LambdaPalette = LambdaPalette
    { lambdaDim :: !V.Attr
    , lambdaTrail :: !V.Attr
    , lambdaGlow :: !V.Attr
    , lambdaSpark :: !V.Attr
    }

lambdaArtWidget :: Int -> Widget Name
lambdaArtWidget elapsedMillis =
    B.Widget B.Fixed B.Fixed do
        context <- B.getContext
        dimAttr <- B.lookupAttrName Theme.lambdaDimAttr
        trailAttr <- B.lookupAttrName Theme.lambdaTrailAttr
        glowAttr <- B.lookupAttrName Theme.lambdaGlowAttr
        sparkAttr <- B.lookupAttrName Theme.lambdaSparkAttr
        let
            composition =
                lambdaComposition context.availWidth context.availHeight
            palette = LambdaPalette
                { lambdaDim = dimAttr
                , lambdaTrail = trailAttr
                , lambdaGlow = glowAttr
                , lambdaSpark = sparkAttr
                }
            rows =
                buildSolidLambdaRows
                    composition.lambdaUpperHeight
                    composition.lambdaLowerHeight
                    composition.lambdaStrokeWidth
            logoWidth = maximum (0 : map length rows)
            logoHeight = length rows
            canvasWidth = logoWidth + 2 * composition.lambdaMarginX
            canvasHeight = logoHeight + 2 * composition.lambdaMarginY
            particles =
                lambdaOrbitParticles
                    palette
                    (elapsedMillis `div` 160)
                    canvasWidth
                    canvasHeight
                    composition.lambdaHasOrbit
            rendered =
                V.vertCat
                    [ V.horizCat
                        [ renderLambdaCell
                            palette
                            elapsedMillis
                            composition
                            rows
                            logoWidth
                            logoHeight
                            particles
                            x
                            y
                        | x <- [0 .. canvasWidth - 1]
                        ]
                    | y <- [0 .. canvasHeight - 1]
                    ]
            bounded =
                V.crop
                    (max 0 context.availWidth)
                    (max 0 context.availHeight)
                    rendered
        pure B.emptyResult { B.image = bounded }

lambdaComposition :: Int -> Int -> LambdaComposition
lambdaComposition width height
    | width >= 42
    , height >= 21 =
        LambdaComposition
            { lambdaUpperHeight = 8
            , lambdaLowerHeight = 11
            , lambdaStrokeWidth = 3
            , lambdaMarginX = 8
            , lambdaMarginY = 1
            , lambdaHasOrbit = True
            }
    | width >= 24
    , height >= 14 =
        LambdaComposition
            { lambdaUpperHeight = 5
            , lambdaLowerHeight = 7
            , lambdaStrokeWidth = 2
            , lambdaMarginX = 4
            , lambdaMarginY = 1
            , lambdaHasOrbit = True
            }
    | otherwise =
        LambdaComposition
            { lambdaUpperHeight = 3
            , lambdaLowerHeight = 4
            , lambdaStrokeWidth = 2
            , lambdaMarginX = 0
            , lambdaMarginY = 0
            , lambdaHasOrbit = False
            }

buildSolidLambdaRows :: Int -> Int -> Int -> [String]
buildSolidLambdaRows upperHeight lowerHeight strokeWidth =
    map row [0 .. totalHeight - 1]
  where
    totalHeight = upperHeight + lowerHeight
    mainStart =
        max 0 (strokeWidth + lowerHeight - 1 - upperHeight)
    width = mainColumn (totalHeight - 1) + strokeWidth
    row rowIndex =
        map (cell rowIndex) [0 .. width - 1]
    cell rowIndex column
        | rowIndex >= upperHeight
        , column >= branchColumn rowIndex
        , column < branchColumn rowIndex + strokeWidth =
            '/'
        | column >= mainColumn rowIndex
        , column < mainColumn rowIndex + strokeWidth =
            '\\'
        | otherwise = ' '
      where
        branchColumn index =
            mainStart
                + upperHeight
                - strokeWidth
                - (index - upperHeight)
    mainColumn index =
        mainStart + index

renderLambdaCell
    :: LambdaPalette
    -> Int
    -> LambdaComposition
    -> [String]
    -> Int
    -> Int
    -> [((Int, Int), Char, V.Attr)]
    -> Int
    -> Int
    -> V.Image
renderLambdaCell
    palette
    elapsedMillis
    composition
    rows
    logoWidth
    logoHeight
    particles
    x
    y =
    case lambdaLogoChar rows composition.lambdaMarginX
        composition.lambdaMarginY x y of
        ' ' ->
            case find
                (\(position, _, _) -> position == (x, y))
                particles of
                Just (_, character, attr) ->
                    V.char attr character
                Nothing ->
                    V.char palette.lambdaDim ' '
        character ->
            let
                localX = x - composition.lambdaMarginX
                localY = y - composition.lambdaMarginY
                (attr, animatedCharacter) =
                    animatedLambdaStroke
                        palette
                        elapsedMillis
                        logoWidth
                        logoHeight
                        localX
                        localY
                        character
            in V.char attr animatedCharacter

lambdaLogoChar :: [String] -> Int -> Int -> Int -> Int -> Char
lambdaLogoChar rows marginX marginY x y
    | localX < 0 || localY < 0 = ' '
    | otherwise =
        case drop localY rows of
            row : _ ->
                case drop localX row of
                    character : _ -> character
                    [] -> ' '
            [] -> ' '
  where
    localX = x - marginX
    localY = y - marginY

animatedLambdaStroke
    :: LambdaPalette
    -> Int
    -> Int
    -> Int
    -> Int
    -> Int
    -> Char
    -> (V.Attr, Char)
animatedLambdaStroke palette elapsedMillis width height x y character =
    ( Theme.interpolateForeground
        palette.lambdaDim
        palette.lambdaSpark
        opacity
    , if opacity >= 0.24
        then energizedStroke character
        else character
    )
  where
    diagonal =
        (fromIntegral x + fromIntegral (max 0 (height - 1 - y)))
            / fromIntegral (max 1 (width + height))
    opacity =
        shineOpacity diagonal (fromIntegral (max 0 elapsedMillis) / 1000)

energizedStroke :: Char -> Char
energizedStroke = \case
    '_' -> '='
    _ -> '*'

lambdaOrbitParticles
    :: LambdaPalette
    -> Int
    -> Int
    -> Int
    -> Bool
    -> [((Int, Int), Char, V.Attr)]
lambdaOrbitParticles _ _ _ _ False = []
lambdaOrbitParticles palette frame width height True =
    [ (position, character, attr)
    | (offset, (character, attr)) <- zip offsets particleStyles
    , Just position <- [cyclicAt path (frame + offset)]
    ]
  where
    path = lambdaOrbitPath width height
    pathLength = length path
    offsets =
        [ 0
        , pathLength `div` 3
        , 2 * pathLength `div` 3
        ]
    particleStyles =
        [ ('*', palette.lambdaSpark)
        , ('+', palette.lambdaGlow)
        , ('.', palette.lambdaTrail)
        ]

lambdaOrbitPath :: Int -> Int -> [(Int, Int)]
lambdaOrbitPath width height =
    top <> right <> bottom <> left
  where
    horizontal = [2, 4 .. width - 3]
    vertical = [2, 4 .. height - 3]
    top = [(x, 0) | x <- horizontal]
    right = [(width - 1, y) | y <- vertical]
    bottom = [(x, height - 1) | x <- reverse horizontal]
    left = [(0, y) | y <- reverse vertical]

cyclicAt :: [a] -> Int -> Maybe a
cyclicAt [] _ = Nothing
cyclicAt values index =
    case drop (index `mod` length values) values of
        value : _ -> Just value
        [] -> Nothing
