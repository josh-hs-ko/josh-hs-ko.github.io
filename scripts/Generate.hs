module Generate where

import Authors
import PermVenues
import Publications

import Data.Function
import Data.List
import Data.Hash.MD5 as MD5
import Text.PrettyPrint
import System.IO.Strict as Strict


publicationType :: a -> a -> a -> PublicationType -> a
publicationType x y z Published   = x
publicationType x y z Warning     = y
publicationType x y z Unpublished = z

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

renderAuthor :: [Author] -> String -> Doc
renderAuthor as n = maybe id (hyperlink . authorURL) (find ((== n) . authorName) as) (text n)

oxfordList :: [Doc] -> Doc
oxfordList []           = empty
oxfordList [d]          = d
oxfordList [d0, d1]     = d0 <+> text "and" <+> d1
oxfordList [d0, d1, d2] = d0 <> comma <+> d1 <> comma <+> text "and" <+> d2
oxfordList (d:ds)       = d <> comma <+> oxfordList ds

renderVenueAndYear :: [PermVenue] -> Maybe (String, Maybe (String, HyperlinkYear)) -> Int -> Doc
renderVenueAndYear pvs Nothing       y = int y
renderVenueAndYear pvs (Just (n, m)) y =
  let addLink = maybe id hyperlink (maybe (fmap permVenueURL (find ((== n) . permVenueName) pvs)) (Just . fst) m)
  in  case maybe ExcludeYear snd m of
        IncludeYear -> addLink (text n <> text "&nbsp;&nbsp;" <> space <> int y)
        ExcludeYear -> addLink (text n) <> text "&nbsp;&nbsp;" <> space <> int y

renderPublication :: [Author] -> [PermVenue] -> Publication -> Doc
renderPublication as pvs p =
  let md5    = take 8 (md5s (MD5.Str (title p ++ concat (authors p) ++ maybe "" fst (venue p) ++ show (year p))))
      pubId  = "publication-"      ++ md5
      infoId = "publication-info-" ++ md5
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
                blockElement "div" [("class", ["col-sm-3"])]
                  (inlineElement "h3" [("class", ["section-title"])] (int (year (head ps)))) $+$
                blockElement "div" [("class", ["col-sm-9"])]
                  (foldr ($+$) empty (map (renderPublication as pvs) ps))) .
  groupBy ((==) `on` year)

process :: String -> String
process inputStr =
  let start = "<!-- AUTO-GENERATED PUBLICATIONS -->"
      end = "<!-- END OF AUTO-GENERATED PUBLICATIONS -->"
      ls = lines inputStr
      (ls0, startLine:ls') = break ((== start) . dropWhile (== ' ')) ls
      indent = length (takeWhile (== ' ') startLine)
      (_, _:ls1) = break ((== end) . dropWhile (== ' ')) ls'
  in  unlines ls0 ++
      render (nest indent $
                text start $+$
                renderPublications authorList permVenueList publicationList $+$
                text end) ++ "\n" ++
      unlines ls1

main :: IO ()
main = let targetFile = "../index.html"
       in  Strict.readFile targetFile >>= writeFile targetFile . process
