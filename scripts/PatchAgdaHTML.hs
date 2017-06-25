import System.IO.Strict as Strict
import System.Environment


main :: IO ()
main = do
  (fileName:_) <- getArgs
  ss <- fmap lines (Strict.readFile fileName)
  let l = length ss
  if drop (l-3) ss == [ "    ></body"
                      , "  ></html"
                      , ">"
                      ]
  then writeFile fileName $ unlines $
         take (l-3) ss ++
           [ "    ><script type=\"text/javascript\">"
           , "       var sc_project=11306427;"
           , "       var sc_invisible=1;"
           , "       var sc_security=\"8a5e32af\";"
           , "       var scJsHost = ((\"https:\" == document.location.protocol) ? \"https://secure.\" : \"http://www.\");"
           , "       document.write(\"<sc\"+\"ript type='text/javascript' src='\" + scJsHost + \"statcounter.com/counter/counter.js'></\"+\"script>\");"
           , "     </script>"
           , "     <noscript>"
           , "       <div class=\"statcounter\">"
           , "         <a title=\"web counter\" href=\"http://statcounter.com/\" target=\"_blank\"><img class=\"statcounter\" src=\"//c.statcounter.com/11306427/0/8a5e32af/1/\" alt=\"web counter\"></a>"
           , "       </div>"
           , "     </noscript></body"
           , "  ></html"
           , ">"
           ]
  else return ()
