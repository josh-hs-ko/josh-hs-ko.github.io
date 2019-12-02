module Gen where

import Authors
import PermVenues
import Publications

import Prelude hiding ((<>))
import Control.Monad
import Data.Function
import Data.Char
import Data.List
import Data.Digest.Pure.MD5 (md5)
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.Text as Text
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime
import Text.PrettyPrint
import System.Directory
import System.Environment
import System.Process
import System.IO
import System.IO.Strict as Strict
import CMark


main :: IO ()
main = do
  args <- getArgs
  guard (length args > 0)
  case args !! 0 of
    "--publications" -> do
      let indexFile = "../index.html"
      writeFile indexFile =<<
        replaceRange "PUBLICATIONS"
          (renderPublications authorList permVenueList publicationList) <$>
          Strict.readFile indexFile
      spawnProcess "open" [indexFile]
      return ()
    "--post" -> do
      guard (length args > 1)
      generatePost (read (args !! 1))

generatePost :: Int -> IO ()
generatePost postNumber = do
  let postNumberStr = fillZeros 4 postNumber
      postListFile  = "post-list.txt"
      templateFile  = "post-template.html"
      postDirectory = "../blog/" ++ postNumberStr ++ "/"
      postFile      = postDirectory ++ postNumberStr ++ ".md"
      htmlFile      = postDirectory ++ "index.html"
      blogIndexFile = "../blog/index.html"
      indexFile     = "../index.html"
  modTime <- utcToLocalTime <$> getCurrentTimeZone
                            <*> getModificationTime postFile
  postList <- read <$> Strict.readFile postListFile
  (title, teaser, post) <-
    extractHeader . commonmarkToNode [] . Text.pack <$>
      Strict.readFile postFile
  let (entryExists, postTime, mRevTime, newPostList) =
        let (ps0, ps1) = span ((> postNumber) . entryNumber) postList
            entry      = head ps1
        in  (not (null ps1) && entryNumber entry == postNumber,
             if entryExists then entryTime entry else modTime,
             if postTime /= modTime then Just modTime else Nothing,
             ps0 ++ PostEntry postNumber postTime title teaser :
             if entryExists then tail ps1 else ps1)
  let post' = insertPostNumber postNumber .
              insertTime postTime mRevTime .
              transformRemark $ post
  writeFile htmlFile =<<
    replaceRange "POST" (text (Text.unpack (nodeToHtml [optUnsafe] post'))) .
    replaceRange "METADATA" (postMetadata title teaser) <$>
      Strict.readFile templateFile
  writeFile blogIndexFile =<<
    replaceRange "POST LIST" (postIndex newPostList) <$>
      Strict.readFile blogIndexFile
  writeFile indexFile =<<
    replaceRange "LATEST BLOG POSTS" (latestIndex (take 3 newPostList)) <$>
      Strict.readFile indexFile
  spawnProcess "open" [indexFile, blogIndexFile, htmlFile]
  regenerate <- getYesOrNo True "Regenerate?"
  if regenerate
  then generatePost postNumber
  else do commit <- if entryExists then return True
                                   else getYesOrNo False "Commit?"
          when commit (writeFile postListFile (show newPostList))

getYesOrNo :: Bool -> String -> IO Bool
getYesOrNo defaultResult prompt = do
  putStr (prompt ++ if defaultResult then " (YES/no) " else " (yes/NO) ")
  hFlush stdout
  response <- getLine
  case map toLower response of
    ""    -> return defaultResult
    "yes" -> return True
    "no"  -> return False
    _     -> getYesOrNo defaultResult prompt

replaceRange :: String -> Doc -> String -> String
replaceRange key doc str =
  let start = "<!-- " ++ key ++ " -->"
      end = "<!-- END OF " ++ key ++ " -->"
      ls = lines str
      (ls0, startLine:ls') = break ((== start) . dropWhile (== ' ')) ls
      indent = length (takeWhile (== ' ') startLine)
      (_, _:ls1) = break ((== end) . dropWhile (== ' ')) ls'
  in  unlines ls0 ++
      render (nest indent $
                text start $+$
                doc $+$
                text end) ++ "\n" ++
      unlines ls1


--------
-- General HTML rendering

element :: String -> [(String, [String])] -> Doc -> (Doc -> Doc -> Doc) -> Doc
element name attrs d andThen =
  (char '<' <>
   foldr (<+>) empty
     (text name :
      map (\(n, as) -> text n <> equals <> doubleQuotes (foldr (<+>) empty (map text as))) attrs) <>
   char '>')
  `andThen` d `andThen` (text"</" <> text name <> char '>')

inlineElement :: String -> [(String, [String])] -> Doc -> Doc
inlineElement name attrs d = element name attrs d (<>)

blockElement :: String -> [(String, [String])] -> Doc -> Doc
blockElement name attrs d = element name attrs (nest 2 d) ($+$)

hyperlink :: String -> Doc -> Doc
hyperlink link = inlineElement "a" [("href", [link])]


--------
-- Publication rendering

oxfordList :: [Doc] -> Doc
oxfordList []           = empty
oxfordList [d]          = d
oxfordList [d0, d1]     = d0 <+> text "and" <+> d1
oxfordList [d0, d1, d2] = d0 <> comma <+> d1 <> comma <+> text "and" <+> d2
oxfordList (d:ds)       = d <> comma <+> oxfordList ds

renderAuthor :: [Author] -> String -> Doc
renderAuthor as n = maybe id (hyperlink . authorURL) (find ((== n) . authorName) as)
                      (text (concat (map (\c -> if isSpace c then "&nbsp;" else [c]) n)))

renderVenueAndYear :: [PermVenue] -> Maybe (String, Maybe (String, HyperlinkYear)) -> Int -> Doc
renderVenueAndYear pvs Nothing       y = int y
renderVenueAndYear pvs (Just (n, m)) y =
  let addLink = maybe id hyperlink (maybe (fmap permVenueURL (find ((== n) . permVenueName) pvs)) (Just . fst) m)
  in  case maybe ExcludeYear snd m of
        IncludeYear -> addLink (text n <> text "&ensp; " <> int y)
        ExcludeYear -> addLink (text n) <> text "&ensp; " <> int y

renderPublication :: [Author] -> [PermVenue] -> Publication -> Doc
renderPublication as pvs p =
  let str    = title p ++ concat (authors p) ++ maybe "" fst (venue p) ++ show (year p)
      md5sum = take 8 . show . md5 . ByteString.pack $ str
      pubId  = "publication-"      ++ md5sum
      infoId = "publication-info-" ++ md5sum
  in  blockElement "div" [("class", ["publication"]), ("id", [pubId])] $
        (blockElement "div" [("class", ["publication-entry"])] $
           blockElement "div" [("class", ["publication-title"])] (hyperlink ('#':pubId) (text (title p))) $+$
           blockElement "div" [("class", ["publication-authors"])] (oxfordList (map (renderAuthor as) (authors p))) $+$
           blockElement "div" [("class", ["publication-venue"])] (renderVenueAndYear pvs (venue p) (year p)) $+$
           (blockElement "div" [("class", ["publication-links"])] $
              (if null (types p)
               then empty
               else foldr ($+$) empty
                      (map (\(pt, str) ->
                              inlineElement "span" [("class", ["publication-link-outer"])] $
                                inlineElement "span"
                                  [("class", ["publication-type", "type-" ++ publicationType "published" "warning" "unpublished" pt])]
                                  (text str))
                           (types p))) $+$
              foldr ($+$) empty
                (map (\(str, mnote, link) ->
                        inlineElement "span" [("class", ["publication-link-outer"])] $
                          inlineElement "a" [("class", ["publication-link"]), ("href", [link])] $
                            text str <> maybe empty (inlineElement "span" [("class", ["link-note"])] . parens . text) mnote)
                     (links p)) $+$
              if null (info p)
              then empty
              else inlineElement "span" [("class", ["publication-link-outer"])]
                     (inlineElement "span" [("class", ["publication-link", "publication-more-info"]),
                                            ("onclick", ["none()"]),
                                            ("data-toggle", ["collapse"]),
                                            ("data-target", ['#':infoId])] (text "More info")))) $+$
        if null (info p)
        then empty
        else blockElement "div" [("class", ["panel", "panel-publication-info", "collapse"]), ("id", [infoId])] $
               blockElement "div" [("class", ["panel-body", "publication-info"])] $
                 foldr ($+$) empty
                   (map (\(n, c, ml) ->
                           blockElement "div" [("class", ["row"])] $
                             blockElement "div" [("class", ["col-sm-2", "publication-info-title"])] (text n) $+$
                             (blockElement "div" [("class", ["col-sm-10"])] $
                               let murl = case (n, c, ml) of
                                            (_, _, Just url) -> Just url
                                            ('D':'O':'I':_, _, _) -> Just ("https://doi.org/" ++ c)
                                            ('a':'r':'X':'i':'v':_, _, _) -> Just ("https://arxiv.org/abs/" ++ c)
                                            (_, 'a':'r':'X':'i':'v':':':arXivNum, _) -> Just ("https://arxiv.org/abs/" ++ arXivNum)
                                            (_, 'h':'t':'t':'p':_, _) -> Just c
                                            _ -> Nothing
                               in  inlineElement "p" [] (maybe id hyperlink murl (text c))))
                        (info p))

renderPublications :: [Author] -> [PermVenue] -> [Publication] -> Doc
renderPublications as pvs =
  foldr ($+$) empty .
  map (\ps -> blockElement "div" [("class",["row"])] $
                (blockElement "div" [("class", ["col-sm-3"])] $
                   inlineElement "h3" [] (int (year (head ps))) $+$
                   inlineElement "div" [("class", ["after-section-title"])] empty) $+$
                blockElement "div" [("class", ["col-sm-9"])]
                  (foldr ($+$) empty (map (renderPublication as pvs) ps))) .
  groupBy ((==) `on` year)


--------
-- Blog post processing

extractHeader :: Node -> (String, String, Node)
extractHeader (Node mp DOCUMENT (ne : ns@(nt : _))) =
  let c = Text.unpack . Text.dropWhileEnd isSpace . nodeToCommonmark [] Nothing
      r = dropWhile isSpace . dropWhile (== '#') . dropWhile isSpace
  in  (r (c nt), c ne, Node mp DOCUMENT ns)

transformRemark :: Node -> Node
transformRemark n@(Node mp DOCUMENT ns) =
  case reverse ns of
    Node mp0 PARAGRAPH ns0 : ns'@(Node _ THEMATIC_BREAK _ : _) ->
      Node mp DOCUMENT $ reverse $
        Node mp0 (CUSTOM_BLOCK (Text.pack "<div class=\"remark\">")
                                (Text.pack "</div>")) ns0 : ns'
    _ -> n

showMonth :: Int -> String
showMonth m =
  ["January", "February", "March", "April", "May", "June", "July", "August",
   "September", "October", "November", "December"] !! (m - 1)

fillZeros :: Int -> Int -> String
fillZeros d n = let s = show n in replicate (d - length s) '0' ++ s

longTime :: LocalTime -> String
longTime (LocalTime d (TimeOfDay hour min _)) =
  let (year, month, day) = toGregorian d
  in  "at " ++ fillZeros 2 hour ++ ":" ++ fillZeros 2 min ++
      " on " ++ show day ++ " " ++ showMonth month ++ " " ++ show year

shortDate :: Day -> String
shortDate d =
  let (year, month, day) = toGregorian d
  in  show day ++ "&nbsp;" ++ take 3 (showMonth month) ++ "&nbsp;’" ++
      drop 2 (show year)

insertPostNumber :: Int -> Node -> Node
insertPostNumber pn (Node mp DOCUMENT ns) =
  Node mp DOCUMENT $
    Node Nothing
      (CUSTOM_BLOCK (Text.pack "<div class=\"post-number\">")
                    (Text.pack "</div>"))
      [Node Nothing (TEXT (Text.pack (fillZeros 4 pn))) []] : ns

insertTime :: LocalTime -> Maybe LocalTime -> Node -> Node
insertTime tp mtr (Node mp DOCUMENT (nh : ns)) =
  let t = "<div class=\"post-time\">Posted " ++ longTime tp ++
          maybe [] ((", and revised " ++) . longTime) mtr ++ "</div>\n"
  in  Node mp DOCUMENT (nh : Node Nothing (HTML_BLOCK (Text.pack t)) [] : ns)

postMetadata :: String -> String -> Doc
postMetadata title teaser =
  text ("<title>" ++ title ++
        " — The trek goes on (Josh Ko’s blog)</title>") $+$
  text ("<meta name=\"description\" content=\"" ++
        concatMap (\c -> case c of '"' -> "\""; _ -> [c]) teaser ++ "\">")

data PostEntry = PostEntry
  { entryNumber :: Int
  , entryTime   :: LocalTime
  , entryTitle  :: String
  , entryTeaser :: String
  } deriving (Show, Read)

cmarkNoNewline :: String -> String
cmarkNoNewline =
  Text.unpack . Text.dropEnd 1 . commonmarkToHtml [] . Text.pack

cmarkNoPara :: String -> String
cmarkNoPara =
  Text.unpack . Text.drop 3 . Text.dropEnd 5 . commonmarkToHtml [] . Text.pack

postIndex :: [PostEntry] -> Doc
postIndex postList =
  foldr ($+$) empty
    [ let postNumberStr = fillZeros 4 (entryNumber entry)
          postUrl       = postNumberStr ++ "/"
      in  blockElement "div" [("class", ["row"])] $
            (inlineElement "div" [("class", ["col-sm-2"])] $
               hyperlink postUrl (text postNumberStr) <>
               text "&nbsp;·&nbsp;" <>
               inlineElement "span" [("class", ["blog-entry-date"])]
                 (text (shortDate (localDay (entryTime entry))))) $+$
            (inlineElement "div" [("class", ["col-sm-3"])] $
               inlineElement "div" [("class", ["blog-entry"])] $
                 hyperlink postUrl $
                   text (cmarkNoPara (entryTitle entry))) $+$
            (inlineElement "div" [("class", ["col-sm-7"])] $
               text (cmarkNoNewline (entryTeaser entry)))
    | entry <- postList ]

latestIndex :: [PostEntry] -> Doc
latestIndex postList =
  foldr ($+$) empty
    [ let postNumberStr = fillZeros 4 (entryNumber entry)
          postUrl       = "blog/" ++ postNumberStr ++ "/"
      in  inlineElement "p" [] $
            (inlineElement "span" [("class", ["paragraph-title"])] $
               hyperlink postUrl (text (cmarkNoPara (entryTitle entry)))) <>
            text (cmarkNoPara (entryTeaser entry)) <>
            (inlineElement "span" [("class", ["blog-entry-stamp"])] $
               text "—&nbsp;" <>
               hyperlink postUrl (text postNumberStr) <>
               text "&nbsp;·&nbsp;" <>
               inlineElement "span" [("class", ["blog-entry-date"])]
                 (text (shortDate (localDay (entryTime entry)))))
    | entry <- postList ]
