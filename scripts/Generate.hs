module Generate where

import Authors
import PermVenues
import Publications

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

renderVenueAndYear :: [PermVenue] -> String -> Maybe (String, HyperlinkYear) -> Int -> Doc
renderVenueAndYear pvs n m y =
  let addLink = maybe id hyperlink (maybe (fmap permVenueURL (find ((== n) . permVenueName) pvs)) (Just . fst) m)
  in  case maybe ExcludeYear snd m of
        IncludeYear -> addLink (inlineElement "span" [("class", ["venue"])] (text n <> space) <> int y)
        ExcludeYear -> inlineElement "span" [("class", ["venue"])] (addLink (text n) <> space) <> int y

renderPublication :: [Author] -> [PermVenue] -> Publication -> Doc
renderPublication as pvs p =
  let md5    = take 8 (md5s (MD5.Str (title p ++ concat (authors p) ++ venue p ++ show (year p))))
      pubId  = "publication-"      ++ md5
      infoId = "publication-info-" ++ md5
  in  (blockElement "div" [("class", ["publication"]), ("id", [pubId])] $
         blockElement "div" [("class", ["publication-title"])] (hyperlink ('#':pubId) (text (title p))) $+$
         blockElement "div" [("class", ["publication-authors"])] (oxfordList (map (renderAuthor as) (authors p))) $+$
         blockElement "div" [("class", ["publication-venue"])] (renderVenueAndYear pvs (venue p) (venueURL p) (year p)) $+$
         (blockElement "div" [("class", ["publication-links"])] $
            maybe empty
              (\(pt, str) -> inlineElement "span"
                               [("class", ["label",
                                           "label-" ++ publicationType "success" "warning" "danger" pt,
                                           "publication-type"])]
                               (text str))
              (pubType p) $+$
            foldr ($+$) empty
              (map (\(str, link) -> inlineElement "span" [("class", ["label", "label-primary"])]
                                      (hyperlink link (text str)))
                 (links p)) $+$
            if null (info p)
            then empty
            else inlineElement "span" [("class", ["label", "label-info", "publication-more-info"]),
                                       ("onclick", ["none()"]),
                                       ("data-toggle", ["collapse"]),
                                       ("data-target", ['#':infoId])] (text "More info"))) $+$
      if null (info p)
      then empty
      else blockElement "div" [("class", ["panel", "panel-publication-info", "collapse"]), ("id", [infoId])] $
             blockElement "div" [("class", ["panel-body"])] $
               blockElement "div" [("class", ["publication-info"])] $
                 foldr ($+$) empty
                   (map (\(n, c, ml) ->
                           blockElement "div" [("class", ["row", "publication-info-entry"])] $
                             blockElement "div" [("class", ["col-sm-2", "publication-info-title"])] (text n) $+$
                             (blockElement "div" [("class", ["col-sm-10"])] $
                               let murl = case (n, c, ml) of
                                            (_, _, Just url) -> Just url
                                            ("DOI", _, _) -> Just ("http://dx.doi.org/" ++ c)
                                            ("arXiv", _, _) -> Just ("https://arxiv.org/abs/" ++ c)
                                            (_, 'h':'t':'t':'p':_, _) -> Just c
                                            _ -> Nothing
                               in  inlineElement "p" [] (maybe id hyperlink murl  (text c))))
                        (info p))

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
                foldr ($+$) empty (map (renderPublication authorList permVenueList) publicationList) $+$
                text end) ++ "\n" ++
      unlines ls1

main :: IO ()
main = let targetFile = "index.html"
       in  Strict.readFile targetFile >>= writeFile targetFile . process
