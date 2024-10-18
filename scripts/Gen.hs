{-# LANGUAGE ViewPatterns #-}

module Gen where

import Authors
import PermVenues
import Publications

import Prelude hiding ((<>))
import Control.Arrow (first, (&&&))
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Char
import Data.Function
import Data.IORef
import Data.List
import Data.Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.String
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime
import Text.PrettyPrint
import Text.Printf
import System.Directory
import System.Environment
import System.Process
import System.IO

import CMark        -- cmark (ACJ5RH83EBH)
import Crypto.Hash  -- crypton (3PD3GBCXTKX)
import Crypto.Random.Types
import Crypto.Cipher.Types
import Crypto.Cipher.AES
import Crypto.Error
import Crypto.Data.Padding


main :: IO ()
main = do
  args <- getArgs
  guard (not (null args))
  case head args of
    "--publications" -> do
      let indexFile = "../index.html"
      postList <- read <$> readFile' postListFile
      writeFile indexFile .
        replaceRange "PUBLICATIONS"
          (renderPublications authorList permVenueList postList publicationList) =<<
          readFile' indexFile
      spawnProcess "open" [indexFile]
      return ()
    "--post" -> do
      guard (length args > 1)
      postList <- read <$> readFile' postListFile
      generatePostInteractively postList (read (args !! 1))
    "--encrypt" -> do
      guard (length args > 1)
      let pn = read (args !! 1)
      encrypted <- doesFileExist (postKeyFile pn)
      if encrypted
      then putStrLn $ "A key already exists for post " ++ postNumberString pn ++ "."
      else do
        createDirectoryIfMissing False (postDir pn)
        writeFile (postKeyFile pn) . toHexString =<< getRandomBytes 32
        sourceExists <- doesFileExist (postSourceFile pn False)
        when sourceExists (renameFile (postSourceFile pn False) (postSourceFile pn True))
        targetExists <- doesFileExist (postTargetFile pn False)
        when targetExists (renameFile (postTargetFile pn False) (postTargetFile pn True))
    "--decrypt" -> do
      guard (length args > 1)
      restorePlaintext (read (args !! 1))
    "--regenerate-posts" -> do
      postList <- read <$> readFile' postListFile
      rPostList <- newIORef postList
      forM_ postList $ \postEntry -> do
        postList' <- readIORef rPostList
        rawPost <- getRawPost (entryNumber postEntry)
        if isEncrypted rawPost
        then do
          let post = processPost postList' rawPost Nothing
          writeHtmlFile post
          maybe (return ()) (writeIORef rPostList) (mNewPostList post)
        else
          writeEncryptedFiles postList' (entryNumber postEntry) rawPost
      finalPostList <- readIORef rPostList
      writeIndexFiles finalPostList
      writePostListFile finalPostList
    _ -> putStrLn "Unrecognised option."


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
  let bytes :: ByteString =
        Text.encodeUtf8 . Text.pack $ title p ++ concat (authors p) ++ maybe "" fst (venue p) ++ show (year p)
      md5sum :: Digest MD5 = hash bytes
      md5str = take 8 (show md5sum)
      pubId  = "publication-"      ++ md5str
      infoId = "publication-info-" ++ md5str
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
      map (\(n, t) -> hyperlink (postUrl n) (text (postNumberString n) <+> parens (text t))) $
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
  , isEncrypted :: Bool
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
  , entryTeaser :: Maybe String
  } deriving (Eq, Show, Read)

postNumberString :: Int -> String
postNumberString = printf "%04d"

postUrl :: Int -> String
postUrl pn = "/blog/" ++ postNumberString pn ++ "/"

postDir :: Int -> String
postDir pn = ".." ++ postUrl pn

postSourceFile :: Int -> Bool -> FilePath
postSourceFile pn encrypted =
  postDir pn ++ (if encrypted then "PLAINTEXT" else postNumberString pn) ++ ".md"

postTargetFile :: Int -> Bool -> FilePath
postTargetFile pn encrypted =
  postDir pn ++ (if encrypted then "PLAINTEXT" else "index") ++ ".html"

getRawPost :: Int -> IO RawPost
getRawPost postNumber = do
  encrypted <- doesFileExist (postKeyFile postNumber)
  modTime <- utcToLocalTime <$> getCurrentTimeZone
                            <*> getModificationTime (postSourceFile postNumber encrypted)
  postContent <- readFile' (postSourceFile postNumber encrypted)
  return (RawPost postNumber postContent modTime encrypted)

processPost :: [PostEntry] -> RawPost -> Maybe [Node] -> ProcessedPost
processPost postList (RawPost postNumber postContent modTime encrypted) mns =
  let (title, teaser, postHeader, postBody) =
        extractHeader . commonmarkToNode [] . Text.pack $ postContent
      (entryExists, postTime, mRevTime, mPostList') =
        let (ps0, ps1) = span ((> postNumber) . entryNumber) postList
            entry      = head ps1
        in  (not (null ps1) && entryNumber entry == postNumber,
             if entryExists then entryTime entry else modTime,
             if diffLocalTime modTime postTime >= 60 then Just modTime else Nothing,
             let newEntry = PostEntry postNumber postTime title
                              (if encrypted then Nothing else Just teaser)
             in  if entryExists && isNothing mRevTime
                 then Nothing
                 else Just (ps0 ++ newEntry :
                            if entryExists then tail ps1 else ps1))
      post' = insertPostNumber postNumber .
              insertTime postTime mRevTime .
              transformDisplayedImage .
              transformRemark $ postHeader (fromMaybe postBody mns)
  in  ProcessedPost post' title (if isNothing mns then teaser else "🔒")
        (postTargetFile postNumber (isNothing mns && encrypted)) mPostList' entryExists

generatePostInteractively :: [PostEntry] -> Int -> IO ()
generatePostInteractively postList postNumber = do
  rawPost <- getRawPost postNumber
  let post' = processPost postList rawPost Nothing
  writeHtmlFile post'
  maybe (return ()) writeIndexFiles (mNewPostList post')
  spawnProcess "open" [indexFile, blogIndexFile, htmlFile post']
  regenerate <- getYesOrNo True "Regenerate?"
  if regenerate
  then generatePostInteractively postList postNumber
  else do
    commit <- if entryExists post'
              then return True
              else getYesOrNo False "Commit?"
    when commit $
      case mNewPostList post' of
        Nothing ->
          writeEncryptedFiles postList postNumber rawPost
        Just newPostList -> do
          writePostListFile newPostList
          writeEncryptedFiles newPostList postNumber rawPost

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
      readFile' templateFile

writeIndexFiles :: [PostEntry] -> IO ()
writeIndexFiles postList = do
  writeFile blogIndexFile .
    replaceRange "POST LIST" (postIndex postList) =<<
      readFile' blogIndexFile
  writeFile indexFile .
    replaceRange "LATEST BLOG POSTS" (latestIndex (take 3 (filter (isJust . entryTeaser) postList))) =<<
      readFile' indexFile

writePostListFile :: [PostEntry] -> IO ()
writePostListFile xs = writeFile postListFile (unlines ("[" : intersperse "," (map show xs) ++ ["]"]))


--------
-- Blog post processing

toCMark :: Node -> String
toCMark = Text.unpack . Text.dropWhileEnd isSpace . nodeToCommonmark [] Nothing

removeHeadingMarking :: String -> String
removeHeadingMarking = dropWhile isSpace . dropWhile (== '#') . dropWhile isSpace

extractHeader :: Node -> (String, String, [Node] -> Node, [Node])
extractHeader (Node mp DOCUMENT (n0@(Node _ (HEADING _) _) : ns)) =
  (removeHeadingMarking (toCMark n0), "", \ns' -> Node mp DOCUMENT (n0 : ns'), ns)
extractHeader (Node mp DOCUMENT (n0@(Node _ PARAGRAPH _) : n1@(Node _ (HEADING _) _) : ns)) =
  (removeHeadingMarking (toCMark n1), toCMark n0, \ns' -> Node mp DOCUMENT (n0 : n1 : ns'), ns)

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
    f (Node mp PARAGRAPH [Node _ (IMAGE srcText _) ns]) =
      let src = Text.unpack srcText
          (mc, ma) = processAlt ns
          new = "<p class=\"image-paragraph\">" ++
                "<a href=\"" ++ src ++ "\"><img " ++
                maybe "class=\"displayed-figure\"" id mc ++
                " src=\"" ++ src ++ "\"" ++
                maybe "" (\s -> " alt=\"" ++ s ++ "\"") ma ++
                "/></a></p>"
      in  Node mp (HTML_BLOCK (Text.pack new)) []
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

longTime :: LocalTime -> String
longTime (LocalTime d (TimeOfDay hour min _)) =
  let (year, month, day) = toGregorian d
  in  "at " ++ printf "%02d" hour ++ ":" ++ printf "%02d" min ++
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
      [Node Nothing (TEXT (Text.pack (postNumberString pn))) []] : ns

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
    [ let pn = entryNumber entry
      in  blockElement "div" [("class", ["row"])] $
            (inlineElement "div" [("class", ["col-sm-2"])] $
               hyperlink (postUrl pn) (text (postNumberString pn)) <>
               text "&nbsp;·&nbsp;" <>
               inlineElement "span" [("class", ["blog-entry-date"])]
                 (text (shortDate (localDay (entryTime entry))))) $+$
            (inlineElement "div" [("class", ["col-sm-3"])] $
               inlineElement "div" [("class", ["blog-entry"])] $
                 hyperlink (postUrl pn) $
                   text (cmarkNoPara (entryTitle entry))) $+$
            (inlineElement "div" [("class", ["col-sm-7"])] $
               text (maybe "🔒" cmarkNoNewline (entryTeaser entry)))
    | entry <- postList ]

latestIndex :: [PostEntry] -> Doc
latestIndex postList =
  foldr ($+$) empty
    [ let pn = entryNumber entry
      in  inlineElement "p" [] $
            (inlineElement "span" [("class", ["paragraph-title"])] $
               hyperlink (postUrl pn) (text (cmarkNoPara (entryTitle entry)))) <>
            text (maybe undefined cmarkNoPara (entryTeaser entry)) <>
            (inlineElement "span" [("class", ["blog-entry-stamp"])] $
               text "—&nbsp;" <>
               hyperlink (postUrl pn) (text (postNumberString pn)) <>
               text "&nbsp;·&nbsp;" <>
               inlineElement "span" [("class", ["blog-entry-date"])]
                 (text (shortDate (localDay (entryTime entry)))))
    | entry <- postList ]


--------
-- Blog encryption

blogKeyFile :: FilePath
blogKeyFile = "PLAINTEXT.txt"

postKeyFile :: Int -> FilePath
postKeyFile pn = postDir pn ++ "PLAINTEXT.txt"

encryptedPostFile :: Int -> FilePath
encryptedPostFile pn = postDir pn ++ postNumberString pn ++ ".ep"

decryptionPage :: ByteString -> ByteString -> String
decryptionPage iv ciphertext =
  "<div id=\"postContent\">\n" ++
  "  <div class=\"container decryption\">\n" ++
  "    <form onsubmit=\"decryptPage(); return false\">\n" ++
  "      <div class=\"form-group\">\n" ++
  "        <input type=\"password\" id=\"postKey\" class=\"form-control\" required autofocus>\n" ++
  "      </div>\n" ++
  "      <div class=\"form-group\">\n" ++
  -- "        <input type=\"submit\" value=\"Decrypt\" class=\"btn btn-primary btn-block\">\n" ++
  "        <button class=\"btn btn-primary btn-block\">Decrypt</button>\n" ++
  "      </div>\n" ++
  "    </form>\n" ++
  "  </div>\n" ++
  "  <script>\n" ++
  "    async function decryptPage() {\n" ++
  "      var iv = \"" ++ toHexString iv ++ "\";\n" ++
  "      var ciphertext = \"" ++ toHexString ciphertext ++ "\";\n" ++
  "      var key = await window.crypto.subtle.importKey(\"raw\", fromHexString(document.getElementById(\"postKey\").value), {name: \"AES-CBC\", length: 256}, false, [\"decrypt\"]);\n" ++
  "      var plaintext = await window.crypto.subtle.decrypt({name: \"AES-CBC\", iv: fromHexString(iv)}, key, fromHexString(ciphertext));\n" ++
  "      document.getElementById(\"postContent\").innerHTML = new TextDecoder(\"UTF-8\").decode(plaintext);\n" ++
  "    }\n" ++
  "    function fromHexString(s) {\n" ++
  "      var r = new Uint8Array(s.length/2);\n" ++
  "      for (var i = 0, j = 0; i < r.length; ++i, j+=2) {\n" ++
  "          r[i] = parseInt(s.substring(j, j+2), 16);\n" ++
  "      }\n" ++
  "      return r;\n" ++
  "    }\n" ++
  "  </script>\n" ++
  "</div>"

encryptPost :: Int -> ByteString -> String -> IO (ByteString, ByteString)
encryptPost pn keyBytes post = do
  let CryptoPassed (key :: AES256) = cipherInit keyBytes
  ivBytes <- getRandomBytes 16
  let Just iv = makeIV ivBytes
  let (_, _, _, ns) = extractHeader (commonmarkToNode [] (Text.pack post))
  let plaintext = pad (PKCS7 16) (Text.encodeUtf8 (nodeToHtml [optUnsafe] (Node Nothing DOCUMENT ns)))
  return (ivBytes, cbcEncrypt key iv plaintext)

writeEncryptedFiles :: [PostEntry] -> Int -> RawPost -> IO ()
writeEncryptedFiles postList postNumber rawPost =
  when (isEncrypted rawPost) $ do
    postKey <- fromHexString <$> readFile' (postKeyFile postNumber)
    (iv, ciphertext) <- encryptPost postNumber postKey (postContent rawPost)
    let post' = processPost postList rawPost
                  (Just [Node Nothing (HTML_BLOCK (Text.pack (decryptionPage iv ciphertext))) []])
    postBytes <- ByteString.readFile (postSourceFile postNumber True)
    let encData = postKey `ByteString.append` postBytes
    CryptoPassed (blogKey :: AES256) <- cipherInit . fromHexString <$> readFile' blogKeyFile
    ivBytes <- getRandomBytes 16
    let Just iv = makeIV ivBytes
    writeHtmlFile post'
    ByteString.writeFile (encryptedPostFile postNumber)
      (ivBytes `ByteString.append` cbcEncrypt blogKey iv (pad (PKCS7 16) encData))

restorePlaintext :: Int -> IO ()
restorePlaintext pn = do
  ep <- ByteString.readFile (encryptedPostFile pn)
  let (ivBytes, ciphertext) = ByteString.splitAt 16 ep
  CryptoPassed (key :: AES256) <- cipherInit . fromHexString <$> readFile' blogKeyFile
  let Just iv = makeIV ivBytes
  let Just encData = unpad (PKCS7 16) (cbcDecrypt key iv ciphertext)
  let (postKeyBytes, postBytes) = ByteString.splitAt 32 encData
  writeFile (postKeyFile pn) (toHexString postKeyBytes)
  ByteString.writeFile (postSourceFile pn True) postBytes

toHexString :: ByteString -> String
toHexString = concatMap (printf "%02x") . ByteString.unpack

fromHexString :: String -> ByteString
fromHexString =
  ByteString.pack .
  unfoldr (\cs -> if null cs then Nothing else Just (read ("0x" ++ take 2 cs), drop 2 cs))


--------
-- List processing utilities from MissingH (AP9ELLBS2QHQ)

spanList :: ([a] -> Bool) -> [a] -> ([a], [a])
spanList _ [] = ([],[])
spanList func list@(x:xs) =
    if func list
       then (x:ys,zs)
       else ([],list)
    where (ys,zs) = spanList func xs

breakList :: ([a] -> Bool) -> [a] -> ([a], [a])
breakList func = spanList (not . func)

split :: Eq a => [a] -> [a] -> [[a]]
split _ [] = []
split delim str =
    let (firstline, remainder) = breakList (isPrefixOf delim) str
        in
        firstline : case remainder of
                                   [] -> []
                                   x -> if x == delim
                                        then [] : []
                                        else split delim
                                                 (drop (length delim) x)
