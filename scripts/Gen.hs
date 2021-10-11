{-# OPTIONS -XViewPatterns #-}

module Gen where

import Authors
import PermVenues
import Publications

import Prelude hiding ((<>))
import Control.Arrow (first, (&&&))
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Data.Char
import Data.Function
import Data.IORef
import Data.List
import Data.Maybe
import qualified Data.Text as Text
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime
import Text.PrettyPrint
import System.Directory
import System.Environment
import System.Process
import System.IO

import Data.List.Utils (split)                -- MissingH
import Data.Digest.Pure.MD5 (md5)             -- pureMD5
import System.IO.Strict as Strict (readFile)  -- strict
import CMark                                  -- cmark

main :: IO ()
main = do
  args <- getArgs
  guard (not (null args))
  case head args of
    "--publications" -> do
      let indexFile = "../index.html"
      postList <- read <$> Strict.readFile postListFile
      writeFile indexFile .
        replaceRange "PUBLICATIONS"
          (renderPublications authorList permVenueList postList publicationList) =<<
          Strict.readFile indexFile
      spawnProcess "open" [indexFile]
      return ()
    "--post" -> do
      guard (length args > 1)
      postList <- read <$> Strict.readFile postListFile
      generatePostInteractively postList (read (args !! 1))
    "--regenerate-posts" -> do
      postList <- read <$> Strict.readFile postListFile
      rPostList <- newIORef postList
      forM_ postList $ \postEntry -> do
        post <- processPost <$> readIORef rPostList
                            <*> getRawPost (entryNumber postEntry)
        writeHtmlFile post
        maybe (return ()) (writeIORef rPostList) (mNewPostList post)
      postList' <- readIORef rPostList
      writeIndexFiles postList'
      writePostListFile postList'
    _ -> return ()


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
-- Publication rendering

oxfordList :: [Doc] -> Doc
oxfordList []           = empty
oxfordList [d]          = d
oxfordList [d0, d1]     = d0 <+> text "and" <+> d1
oxfordList [d0, d1, d2] = d0 <> comma <+> d1 <> comma <+> text "and" <+> d2
oxfordList (d:ds)       = d <> comma <+> oxfordList ds

renderAuthor :: [Author] -> String -> Doc
renderAuthor as n = maybe id (hyperlink . authorURL) (find ((== n) . authorName) as)
                      (text (concatMap (\c -> if isSpace c then "&nbsp;" else [c]) n))

renderVenueAndYear :: [PermVenue] -> Maybe (String, Maybe (String, HyperlinkYear)) -> Int -> Doc
renderVenueAndYear pvs Nothing       y = int y
renderVenueAndYear pvs (Just (n, m)) y =
  let addLink = maybe id hyperlink (maybe (fmap permVenueURL (find ((== n) . permVenueName) pvs)) (Just . fst) m)
  in  case maybe ExcludeYear snd m of
        IncludeYear -> addLink (text n <> text "&ensp; " <> int y)
        ExcludeYear -> addLink (text n) <> text "&ensp; " <> int y

renderPublication :: [Author] -> [PermVenue] -> [PostEntry] -> Publication -> Doc
renderPublication as pvs postList p =
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
                               inlineElement "p" [] (processInfoEntry n c ml postList)))
                        (info p))
  where
    processInfoEntry :: String -> String -> Maybe String -> [PostEntry] -> Doc
    processInfoEntry _ c (Just url) _ = hyperlink url (text c)
    processInfoEntry ((== "DOI") -> True) c _ _ = hyperlink ("https://doi.org/" ++ c) (text c)
    processInfoEntry ((== "arXiv") -> True) c _ _ = hyperlink ("https://arxiv.org/abs/" ++ c) (text c)
    processInfoEntry (("Related blog post" `isPrefixOf`) -> True) c _ postList =
      foldr (<+>) empty $
      punctuate (char ',') $
      map (\(n, t) -> let numStr = fillZeros 4 n
                      in  hyperlink ("/blog/" ++ numStr ++ "/") (text numStr <+> parens (text t))) $
      reverse $
      filter ((`elem` [ read n | n <- split ", " c ]) . fst) $
      map (entryNumber &&& entryTitle) postList
    processInfoEntry _ c@(first (== "arXiv:") . splitAt 6 -> (True, arXivNum)) _ _ =
      hyperlink ("https://arxiv.org/abs/" ++ arXivNum) (text c)
    processInfoEntry _ c@(("http" `isPrefixOf`) -> True) _ _ = hyperlink c (text c)
    processInfoEntry _ c _ _ = text c


renderPublications :: [Author] -> [PermVenue] -> [PostEntry] -> [Publication] -> Doc
renderPublications as pvs postList =
  foldr ($+$) empty .
  map (\ps -> blockElement "div" [("class",["row"])] $
                (blockElement "div" [("class", ["col-sm-3"])] $
                   inlineElement "h3" [("class", ["year-group"])] (int (year (head ps))) $+$
                   inlineElement "div" [("class", ["after-section-title"])] empty) $+$
                blockElement "div" [("class", ["col-sm-9"])]
                  (foldr ($+$) empty (map (renderPublication as pvs postList) ps))) .
  groupBy ((==) `on` year)


--------
-- Blog post IO

postListFile, templateFile, blogIndexFile, indexFile :: FilePath
postListFile = "post-list.txt"
templateFile = "post-template.html"
blogIndexFile = "../blog/index.html"
indexFile     = "../index.html"

data RawPost = RawPost
  { postNumber  :: Int
  , postContent :: String
  , modTime     :: LocalTime
  , targetFile  :: FilePath
  }

data ProcessedPost = ProcessedPost
  { postNode     :: Node
  , postTitle    :: String
  , postTeaser   :: String
  , htmlFile     :: FilePath
  , mNewPostList :: Maybe [PostEntry]
  , entryExists  :: Bool
  }

data PostEntry = PostEntry
  { entryNumber :: Int
  , entryTime   :: LocalTime
  , entryTitle  :: String
  , entryTeaser :: String
  } deriving (Eq, Show, Read)

getRawPost :: Int -> IO RawPost
getRawPost postNumber = do
  let postNumberStr = fillZeros 4 postNumber
      postDirectory = "../blog/" ++ postNumberStr ++ "/"
      postFile      = postDirectory ++ postNumberStr ++ ".md"
      targetFile    = postDirectory ++ "index.html"
  modTime <- utcToLocalTime <$> getCurrentTimeZone
                            <*> getModificationTime postFile
  postContent <- Strict.readFile postFile
  return (RawPost postNumber postContent modTime targetFile)

processPost :: [PostEntry] -> RawPost -> ProcessedPost
processPost postList (RawPost postNumber postContent modTime targetFile) =
  let (title, teaser, post) =
        extractHeader . commonmarkToNode [] . Text.pack $ postContent
      (entryExists, postTime, mRevTime, mPostList') =
        let (ps0, ps1) = span ((> postNumber) . entryNumber) postList
            entry      = head ps1
        in  (not (null ps1) && entryNumber entry == postNumber,
             if entryExists then entryTime entry else modTime,
             if diffLocalTime modTime postTime >= 60 then Just modTime else Nothing,
             let newEntry = PostEntry postNumber postTime title teaser
             in  if entryExists && isNothing mRevTime
                 then Nothing
                 else Just (ps0 ++ newEntry :
                            if entryExists then tail ps1 else ps1))
      post' = insertPostNumber postNumber .
              insertTime postTime mRevTime .
              transformDisplayedImage .
              transformRemark $ post
  in  ProcessedPost post' title teaser targetFile mPostList' entryExists

generatePostInteractively :: [PostEntry] -> Int -> IO ()
generatePostInteractively postList postNumber = do
  rawPost <- getRawPost postNumber
  let post' = processPost postList rawPost
  writeHtmlFile post'
  maybe (return ()) writeIndexFiles (mNewPostList post')
  spawnProcess "open" [indexFile, blogIndexFile, htmlFile post']
  regenerate <- getYesOrNo True "Regenerate?"
  if regenerate
  then generatePostInteractively postList postNumber
  else do commit <- if entryExists post'
                    then return True
                    else getYesOrNo False "Commit?"
          case mNewPostList post' of
            Nothing -> return ()
            Just postList' ->
              when commit (writePostListFile postList')

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

writeHtmlFile :: ProcessedPost -> IO ()
writeHtmlFile (ProcessedPost post title teaser htmlFile _ _) =
  writeFile htmlFile .
    replaceRange "POST" (text (Text.unpack (nodeToHtml [optUnsafe] post))) .
    replaceRange "METADATA" (postMetadata title teaser) =<<
      Strict.readFile templateFile

writeIndexFiles :: [PostEntry] -> IO ()
writeIndexFiles postList = do
  writeFile blogIndexFile .
    replaceRange "POST LIST" (postIndex postList) =<<
      Strict.readFile blogIndexFile
  writeFile indexFile .
    replaceRange "LATEST BLOG POSTS" (latestIndex (take 3 postList)) =<<
      Strict.readFile indexFile

writePostListFile :: [PostEntry] -> IO ()
writePostListFile xs = writeFile postListFile (unlines ("[" : intersperse "," (map show xs) ++ ["]"]))


--------
-- Blog post processing

extractHeader :: Node -> (String, String, Node)
extractHeader (Node mp DOCUMENT (ne : ns@(nt : _))) =
  let c = Text.unpack . Text.dropWhileEnd isSpace . nodeToCommonmark [] Nothing
      r = dropWhile isSpace . dropWhile (== '#') . dropWhile isSpace
  in  (r (c nt), c ne, Node mp DOCUMENT ns)

transformDisplayedImage :: Node -> Node
transformDisplayedImage (Node mp DOCUMENT ns) = Node mp DOCUMENT (map f ns)
  where
    processAlt' :: [Node] -> Maybe String
    processAlt' [n] = Just (Text.unpack (nodeToHtml [optUnsafe] n))
    processAlt' _   = Nothing
    processAlt  :: [Node] -> (Maybe String, Maybe String)
    processAlt (Node _ (CODE t) _ : ns) = (Just (Text.unpack t), processAlt' ns)
    processAlt ns                       = (Nothing             , processAlt' ns)
    f :: Node -> Node
    f (Node mp0 PARAGRAPH [Node mp1 (IMAGE srcText _) ns]) =
      let src = Text.unpack srcText
          (mc, ma) = processAlt ns
          img = "<a href=\"" ++ src ++ "\"><img " ++
                maybe "class=\"displayed-figure\"" id mc ++
                " src=\"" ++ src ++ "\"" ++
                maybe "" (\s -> " alt=\"" ++ s ++ "\"") ma ++
                "/></a>"
      in  Node mp0 PARAGRAPH [Node mp1 (HTML_INLINE (Text.pack img)) []]
    f n = n

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
          postUrl       = "/blog/" ++ postNumberStr ++ "/"
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
          postUrl       = "/blog/" ++ postNumberStr ++ "/"
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
