{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module Main where

import Data.List (foldl', nub, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Text.Printf
import GHC.CmmToAsm.AArch64.Instr (_d)

main :: IO ()
main = putStr "Hello, world!"

data Config = MkConfig
  { currentWeek :: Int, -- current week [releases content fully visible up to this week]
    activityNum :: Int, -- number of activities per week (empty slots possible)
    columnNum :: Int, -- desired columns per week (yet, autofitted to max 2 rows per week)
    title :: String, -- content title (different to unitName since multiple content streams maybe in one unit)
    headerOn :: Bool, -- table column headers on(=1) or off(=0), min of 4 columns needed to render
    header1 :: String, -- leftmost 1x column header
    header2 :: String, -- middle 2x column header
    header3 :: String, -- rest of the columns header
    inactColour :: Colour, -- font colour for inactive content
    titleColour :: Colour, -- table title colour
    titleBColour :: Colour, -- table title background colour
    bkgColour :: Colour, -- table border background colour
    embossColour :: Colour, -- table border emboss colour
    fontSizePix :: Int -- font size in pixels
    --   , extendCatNum1 :: Int     -- number of one category that has no border to above cell (e.g. for multi-week coursework)
    --   , extendCatNum2 :: Int     -- number of one category that has no border to above cell (e.g. for multi-week empty)
  }
  deriving (Show)

-- extendCatNum1 and extendCatNum2 should be generated

type Colour = String

config :: Config
config =
  MkConfig
    { currentWeek = 3,
      activityNum = 8,
      columnNum = 4,
      title = "FUNCTIONAL PROGRAMMING",
      headerOn = True,
      header1 = "EXERCISES",
      header2 = "LECTURES",
      header3 = "COURSEWORK",
      inactColour = "#999999",
      titleColour = "#777777",
      titleBColour = "#BBBBBB",
      bkgColour = "#CCCCCC",
      embossColour = "#AAAAAA",
      fontSizePix = 11
    }

data GridEntry = Entry
  { title :: String, -- Title
    spec :: EntrySpec,
    materials :: [MaterialLink]
  }

data EntrySpec
  = ExtraMaterials
  | Lectures
      { slidesFile :: String
      , revisionVideos :: [URL]
      }
  | SetupLab
      { setupLink :: URL }
  | Worksheet
      { file :: String }
  | History
  | NotesExtra
  | Coursework
  | Blank
  deriving (Show, Eq, Ord)

type Week = [GridEntry]

type Page = [Week]

type URL = String

type SlidesLink = String

data MaterialLink = MkMaterial
  { mtype :: MaterialType,
    name :: String,
    file :: String
  }
  deriving (Show, Eq, Ord)

data MaterialType = Note | Sheet | External
  deriving (Show, Eq, Ord)

note :: String -> String -> MaterialLink
note = MkMaterial Note

sheet :: String -> MaterialLink
sheet file = MkMaterial Sheet file file

sheets :: Int -> [MaterialLink]
sheets i = map sheet
  [ printf "sheet%02d.pdf" i
  , printf "sheet%02dDyslexic.pdf" i
  ]

answers :: Int -> [MaterialLink]
answers i = map sheet
  [ printf "answer%02d.pdf" i
  , printf "answer%02dDyslexic.pdf" i
  ]

external :: String -> String -> MaterialLink
external = MkMaterial External

functionalPage :: Page
functionalPage =
  [ -- Week 1
    [  Entry
        { title = "",
          spec = ExtraMaterials,
          materials = 
            [ external "Guest seminar VOD: Haskell in the Datacentre" "https://web.microsoftstream.com/video/17f0fbf7-461c-4cf1-937f-21e8407a137e"
            , external "Paper: How functional programming mattered" "https://mengwangoxf.github.io/Papers/NSR15.pdf"
            , external "Bristol PL Research Group" "https://bristolpl.github.io/"
            ]
        }
    , Entry
        { title = "Week 1 - Introduction",
          spec =
            Lectures
              { slidesFile = "week1.pdf",
                revisionVideos = ["https://mediasite.bris.ac.uk/Mediasite/Play/18e6ea68ad654e9aaafc9f34805f2c831d"]
              },
          materials = []
        }
    , Entry
        { title = "GET YOUR PC READY",
          spec = SetupLab{setupLink = bbRootDir ++ "setup.html"},
          materials = []
        }
    ]
    -- Week 2
  , [ Entry
        { title = "Week 2 - Data Types and Functions",
          spec =
            Lectures
              { slidesFile = "week2.pdf",
                revisionVideos = [ "https://mediasite.bris.ac.uk/Mediasite/Play/21b78fbe973d43a599fbf79dd94f8aa51d"
                                 , "https://mediasite.bris.ac.uk/Mediasite/Play/7aae664dcbf94eb28fe13a6ed93f24221d"
                                 ]
              },
          materials = []
        }
    , Entry
        { title = "Types, Parentheses, and Inhabitants",
          spec =
            Worksheet
              { file = "week2.pdf" },
          materials = sheets 1 ++ answers 1
        }
    ]
  ]

data Category = MkCat
  { title :: String,
    colour :: Colour,
    counter :: Bool,
    slidesLinkName :: String,
    materialLinkName :: String
  }
  deriving (Show, Eq, Ord)

gridEntryCategory :: GridEntry -> Category
gridEntryCategory (Entry _ details materials) = case details of
  Lectures{} -> simpleCat "Lectures" "#CCCFFF"
  ExtraMaterials -> simpleCat "Extra Materials" "#DDDDDD"
  SetupLab{} -> simpleCat "Setup Lab:" "#CCCCCC"
  Worksheet{} -> MkCat
        { title = "Worksheet"
        , colour = "#EEEEDD"
        , counter = True
        , slidesLinkName = ""
        , materialLinkName = "Materials"
        }
  _ -> blankCategory
  where
    simpleCat title colour = MkCat
        { title,
          colour,
          counter = False,
          slidesLinkName = "",
          materialLinkName = ""
        }

type CategoryDict = Map Category Int

genCategoryDict :: Page -> CategoryDict
genCategoryDict page =
  concat page
    |> map gridEntryCategory
    |> nub
    |> (`zip` [1 ..])
    |> M.fromList
    |> M.insert blankCategory 0

blankCategory :: Category
blankCategory =
  MkCat
    { title = "",
      colour = "",
      counter = False,
      slidesLinkName = "",
      materialLinkName = ""
    }

-- genMaterials :: Page -> MaterialDict

data Activity = MkActivity
  { categoryNum :: Int,
    dateTime :: String,
    title :: String,
    activityURL :: String,
    slidesURL :: String,
    materialStart :: Int,
    materialRange :: Int
  } deriving (Show, Eq, Ord)

data FoldData = MkFD

data ActivitiesMaterials = MkAM ![Activity] ![MaterialLink]
  deriving (Show, Eq, Ord)

instance Semigroup ActivitiesMaterials where
  (MkAM a1s m1s) <> (MkAM a2s m2s) = MkAM (a1s ++ a2s') (m1s ++ m2s)
    where
      a2s' = map (adjustIndex (length m1s)) a2s
      adjustIndex n activity@(MkActivity {materialStart}) =
        activity {materialStart = materialStart + n}

instance Monoid ActivitiesMaterials where
  mempty = MkAM [] []

blankEntry :: GridEntry
blankEntry = Entry "" Blank []

genActivitiesAndMaterials
  :: Page
  -> CategoryDict
  -> Config
  -> ActivitiesMaterials
genActivitiesAndMaterials page catDict config
  =  map genPerWeek page
  |> mconcat
  where
    genPerWeek :: Week -> ActivitiesMaterials
    genPerWeek gridEntries =
      [e1, e2, e3, e4, e5, e6, e7, e8]
        |> map genPerEntry
        |> mconcat
      where
        ([e1, e5], [e2, e3, e6, e7], [e4, e8]) = (pad 2 exercises, pad 4 lectures, pad 2 coursework)
        (exercises, lectures, coursework) = foldr splitIntoSections ([], [], []) gridEntries

        splitIntoSections entry@(Entry {spec}) (ex, lecs, cws)
          | isLectureCategory spec = (ex, entry : lecs, cws)
          | isCourseworkCategory spec = (ex, lecs, entry : cws)
          | otherwise = (entry : ex, lecs, cws)

        isLectureCategory x = case x of
          Lectures {} -> True
          NotesExtra {} -> True
          _ -> False

        isCourseworkCategory x = case x of
          Worksheet {} -> True
          SetupLab {} -> True
          Coursework {} -> True
          _ -> False

        pad :: Int -> [GridEntry] -> [GridEntry]
        pad n xs = take n $ xs ++ repeat blankEntry

    genPerEntry :: GridEntry -> ActivitiesMaterials
    genPerEntry entry@(Entry {title, spec, materials}) =
      MkAM [activity] materials
      where
        activity =
          MkActivity
            { categoryNum = catDict M.! (gridEntryCategory entry), -- Slightly unsafe
              dateTime = case spec of
                ExtraMaterials -> "(optional)"
                SetupLab{} -> "Thurs 29/09/22<br/>15:00-18:00<br/>MVB2.11/1.15"
                Worksheet{} -> labTime
                Lectures{} -> "Mon 11:00-11:50<br/>Tues 14:00-14:50<br/>QB1.40 Pugsley"
                _ -> "",
              title = title,
              activityURL = case spec of
                Lectures{slidesFile} -> slideLink slidesFile
                SetupLab{setupLink}  -> setupLink
                Worksheet{file} -> sheetLink file
                _ -> "",
              slidesURL = case spec of
                _ -> "",
              materialStart = 0,
              materialRange = length materials
            }
        
        labTime = "Thurs 15:00-18:00<br/>MVB2.11/1.15"

    blankActivity :: Activity
    blankActivity =
      MkActivity
        { categoryNum = 0,
          dateTime = "",
          title = "",
          activityURL = "",
          slidesURL = "",
          materialStart = 0,
          materialRange = 0
        }

-- >>> genActivitiesAndMaterials functionalPage functionalCatDict config
-- MkAM [MkActivity {categoryNum = 0, dateTime = "", title = "", activityURL = "", slidesURL = "", materialStart = 0, materialRange = 0},MkActivity {categoryNum = 1, dateTime = "", title = "Week 1 - Introduction", activityURL = "https://www.ole.bris.ac.uk/bbcswebdav/courses/COMS10016_2022_TB-1/content/functional/sheets/week1.pdf", slidesURL = "", materialStart = 0, materialRange = 0},MkActivity {categoryNum = 0, dateTime = "", title = "", activityURL = "", slidesURL = "", materialStart = 0, materialRange = 0},MkActivity {categoryNum = 0, dateTime = "", title = "", activityURL = "", slidesURL = "", materialStart = 0, materialRange = 0},MkActivity {categoryNum = 0, dateTime = "", title = "", activityURL = "", slidesURL = "", materialStart = 0, materialRange = 0},MkActivity {categoryNum = 0, dateTime = "", title = "", activityURL = "", slidesURL = "", materialStart = 0, materialRange = 0},MkActivity {categoryNum = 0, dateTime = "", title = "", activityURL = "", slidesURL = "", materialStart = 0, materialRange = 0},MkActivity {categoryNum = 0, dateTime = "", title = "", activityURL = "", slidesURL = "", materialStart = 0, materialRange = 0}] []

functionalCatDict :: CategoryDict
functionalCatDict = genCategoryDict functionalPage

bbRootDir :: String
bbRootDir = "https://www.ole.bris.ac.uk/bbcswebdav/courses/COMS10016_2022_TB-1/"

funcRootDir :: String
funcRootDir = bbRootDir ++ "content/functional/"

dir :: String -> String -> String
dir folder path = funcRootDir ++ folder ++ "/" ++ path

sheetLink :: String -> String
sheetLink = dir "sheets"

noteLink :: String -> String
noteLink = dir "notes"

slideLink :: String -> String
slideLink = dir "slides"

-- >>> genCategoryDict functionalPage
-- fromList [(MkCat {title = "Lectures", colour = "#CCCFFF", counter = False, slidesLink = "", materialLink = ""},0)]

-- Compile to JS

pageToJS :: Page -> Config -> String
pageToJS page config = unlines
  [ configToJS config
  , categoriesToJS catDict
  , activitiesToJS activities
  , materialsToJS materials
  ]
  where
    catDict = genCategoryDict page
    MkAM activities materials
      = genActivitiesAndMaterials page catDict config


configToJS :: Config -> String
configToJS MkConfig{..} =
  [ ("currentWeek  ", show currentWeek  )
  , ("activityNum  ", show activityNum  )
  , ("columnNum    ", show columnNum    )
  , ("title        ", show title        )
  , ("headerOn     ", show headerOn     )
  , ("header1      ", show header1      )
  , ("header2      ", show header2      )
  , ("header3      ", show header3      )
  , ("inactColour  ", show inactColour  )
  , ("titleColour  ", show titleColour  )
  , ("titleBColour ", show titleBColour )
  , ("bkgColour    ", show bkgColour    )
  , ("embossColour ", show embossColour )
  , ("fontSizePix  ", show fontSizePix  )
  , ("extendCatNum1", "-1")
  , ("extendCatNum2", "-1")
  ]
  |> map (\(name,val) -> "const " ++ name ++ " = " ++ val ++ ";")
  |> unlines

categoriesToJS :: CategoryDict -> String
categoriesToJS catDict
  = unlines
  [ "var categories = ["
  , M.toList catDict
    |> sortOn snd
    |> map (\(MkCat{..}, index) -> listToJSArray [show index, title, colour, if counter then "1" else "0", slidesLinkName, materialLinkName])
    |> unlines
  , "];"
  ]


listToJSArray :: [String] -> String
listToJSArray xs = "[" ++ foldr f "" xs ++ "],"
  where
    f listItem acc = show listItem ++ "," ++ acc

activitiesToJS :: [Activity] -> String
activitiesToJS activities
  = unlines
  [ "const activities = ["
  , activities
    |> map (\MkActivity{..} -> listToJSArray
               [show categoryNum, dateTime, title, activityURL, slidesURL, show materialStart, show materialRange])
    |> unlines
  , "];"
  ]

materialsToJS :: [MaterialLink] -> String
materialsToJS materials
  = unlines
  [ "const files = ["
  , zipWith materialToJS [0..] materials
    |> unlines
  , "];"
  ]

materialToJS :: Int -> MaterialLink -> String
materialToJS index MkMaterial{..}
  = listToJSArray
  $ [show index, name]
  ++ [case mtype of
        Note -> noteLink file
        Sheet -> sheetLink file
        External -> file
     ]

-- Utility functions

infixl 0 |>

(|>) :: a -> (a -> b) -> b
x |> f = f x
