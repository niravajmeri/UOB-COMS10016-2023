{-# language DuplicateRecordFields, NamedFieldPuns #-}
module Main where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.List (foldl')

main :: IO ()
main = putStr "Hello, world!"

data Config = MkConfig
  { currentWeek   :: Int     -- current week [releases content fully visible up to this week]
  , activityNum   :: Int     -- number of activities per week (empty slots possible)
  , columnNum     :: Int     -- desired columns per week (yet, autofitted to max 2 rows per week)
  , title         :: String  -- content title (different to unitName since multiple content streams maybe in one unit)
  , headerOn      :: Bool    -- table column headers on(=1) or off(=0), min of 4 columns needed to render
  , header1       :: String  -- leftmost 1x column header
  , header2       :: String  -- middle 2x column header
  , header3       :: String  -- rest of the columns header
  , inactColour   :: Colour  -- font colour for inactive content
  , titleColour   :: Colour  -- table title colour
  , titleBColour  :: Colour  -- table title background colour
  , bkgColour     :: Colour  -- table border background colour
  , embossColour  :: Colour  -- table border emboss colour
  , fontSizePix   :: Int     -- font size in pixels
--   , extendCatNum1 :: Int     -- number of one category that has no border to above cell (e.g. for multi-week coursework)
--   , extendCatNum2 :: Int     -- number of one category that has no border to above cell (e.g. for multi-week empty)
  } deriving (Show)
-- extendCatNum1 and extendCatNum2 should be generated

type Colour = String

config :: Config
config = MkConfig
  { currentWeek   = 3
  , activityNum   = 8
  , columnNum     = 4
  , title         = "FUNCTIONAL PROGRAMMING"
  , headerOn      = True
  , header1       = "EXERCISES"
  , header2       = "LECTURES"
  , header3       = "COURSEWORK"
  , inactColour   = "#999999"
  , titleColour   = "#777777"
  , titleBColour  = "#BBBBBB"
  , bkgColour     = "#CCCCCC"
  , embossColour  = "#AAAAAA"
  , fontSizePix   = 11
  }

data GridEntry = Entry
  { title :: String -- Title
  , spec :: EntrySpec
  , materials :: [MaterialLink]
  }

data EntrySpec
  = ExtraMaterials 
  | Lectures
    { slidesFile :: SlidesLink
    , revisionVideos :: [URL]
    }
  | SetupLab
  | Worksheet
  | History
  | NotesExtra
  | Coursework
  deriving (Show, Eq, Ord)

type Week = [GridEntry]
type Page = [Week]
type URL  = String
type SlidesLink = String

data MaterialLink
  = Notes
    { name :: String
    , file :: String
    }
  | Sheet
    { name :: String
    , file :: String
    }
  | External
    { name :: String
    , url  :: String
    }
  deriving (Show, Eq, Ord)

functionalPage :: Page
functionalPage =
  [ -- Week 1
    [ Entry
        { title = "Week 1 - Introduction"
        , spec = Lectures 
            { slidesFile = "week1.pdf"
            , revisionVideos = ["https://mediasite.bris.ac.uk/Mediasite/Play/18e6ea68ad654e9aaafc9f34805f2c831d"]
            }
        , materials = []
        }
    ]
  ]

data Category = MkCat
  { title :: String
  , colour :: Colour
  , counter :: Bool
  , slidesLinkName :: String
  , materialLinkName :: String
  } deriving (Show, Eq, Ord)

gridEntryCategory :: GridEntry -> Category
gridEntryCategory (Entry _ details materials) = case details of
  Lectures{} -> lectureCat

  where
    lectureCat = MkCat
      { title = "Lectures"
      , colour = "#CCCFFF"
      , counter = False
      , slidesLinkName = ""
      , materialLinkName = ""
      }

type CategoryDict = Map Category Int

genCategoryDict :: Page -> CategoryDict
genCategoryDict page
  =  concat page
  |> map gridEntryCategory
  |> (`zip` [1..])
  |> M.fromList
  |> M.insert blankCategory 0

blankCategory :: Category
blankCategory = undefined

-- genMaterials :: Page -> MaterialDict


data Activity = MkActivity
  { categoryNum :: Int
  , dateTime :: String
  , title :: String
  , activityURL :: String
  , slidesURL :: String
  , materialStart :: Int
  , materialRange :: Int 
  }


data FoldData = MkFD
data ActivitiesMaterials = MkAM ![Activity] ![MaterialLink]

instance Semigroup ActivitiesMaterials where
  (MkAM a1s m1s) <> (MkAM a2s m2s) = MkAM (a1s ++ a2s') (m1s ++ m2s)
    where
      a2s' = map (adjustIndex (length m1s)) a2s
      adjustIndex n activity@(MkActivity{materialStart})
        = activity{materialStart = materialStart + n} 

instance Monoid ActivitiesMaterials where
  mempty = MkAM [] []


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
    genPerWeek gridEntries = undefined
      where
        (exercises, lectures, coursework) = foldr splitIntoSections ([],[],[]) gridEntries

        splitIntoSections entry@(Entry{spec}) (ex, lecs, cws)
          | isLectureCategory spec    = (ex, entry:lecs, cws)
          | isCourseworkCategory spec = (ex, lecs, entry:cws)
          | otherwise                 = (entry:ex, lecs, cws)

        isLectureCategory x = case x of
          Lectures{} -> True
          NotesExtra{} -> True
          _ -> False

        isCourseworkCategory x = case x of
          Worksheet{} -> True
          SetupLab{} -> True
          Coursework{} -> True
          _ -> False
          

    genPerEntry :: GridEntry -> ActivitiesMaterials
    genPerEntry entry@(Entry {title, spec, materials})
      = MkAM [activity] materials
      where
        activity = MkActivity
          { categoryNum = catDict M.! (gridEntryCategory entry) -- Slightly unsafe
          , dateTime = case spec of 
              _ -> ""
          , title = title
          , activityURL = case spec of
              Lectures{slidesFile} -> sheet slidesFile
          , slidesURL = case spec of
              _ -> ""
          , materialStart = 0
          , materialRange = length materials
          }

    blankActivity :: Activity
    blankActivity = MkActivity
      { categoryNum = 0
      , dateTime = ""
      , title = ""
      , activityURL = ""
      , slidesURL = ""
      , materialStart = 0
      , materialRange = 0
      }

rootDir :: String
rootDir = "https://www.ole.bris.ac.uk/bbcswebdav/courses/COMS10016_2022_TB-1/content/functional/"

dir :: String -> String -> String
dir folder path = rootDir ++ folder ++ "/" ++ path

sheet :: String -> String
sheet = dir "sheets"

notes :: String -> String
notes = dir "notes"

slides :: String -> String
slides = dir "slides"

-- >>> genCategoryDict functionalPage 
-- fromList [(MkCat {title = "Lectures", colour = "#CCCFFF", counter = False, slidesLink = "", materialLink = ""},0)]

-- Utility functions 

infixl 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x
