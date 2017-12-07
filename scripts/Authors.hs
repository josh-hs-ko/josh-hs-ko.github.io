module Authors where


data Author = Author
  { authorName :: String
  , authorURL  :: String
  }


authorList :: [Author]
authorList =
  [ Author
      { authorName = "Anthony Anjorin"
      , authorURL  = "https://anthonyanjorin.github.io"
      }
  , Author
      { authorName = "Zinovy Diskin"
      , authorURL  = "http://www.cs.toronto.edu/~zdiskin/"
      }
  , Author
      { authorName = "Jeremy Gibbons"
      , authorURL  = "http://www.cs.ox.ac.uk/jeremy.gibbons/"
      }
  , Author
      { authorName = "Zhenjiang Hu"
      , authorURL  = "http://research.nii.ac.jp/~hu/"
      }
  , Author
      { authorName = "Patrik Jansson"
      , authorURL  = "http://www.cse.chalmers.se/~patrikj/"
      }
  , Author
      { authorName = "Frédéric Jouault"
      , authorURL  = "http://www.eseo.fr/annuaire-du-personnel/entry-5239-jouault-frederic.html"
      }
  , Author
      { authorName = "Erhan Leblebici"
      , authorURL  = "http://www.es.tu-darmstadt.de/es/team/erhan-leblebici/"
      }
  , Author
      { authorName = "Pedro Martins"
      , authorURL  = "https://isr.uci.edu/users/pedro-ribeiro-martins"
      }
  , Author
      { authorName = "Jorge Mendes"
      , authorURL  = "http://wiki.di.uminho.pt/twiki/bin/view/Personal/JorgeMendes/WebHome"
      }
  , Author
      { authorName = "Shin-Cheng Mu"
      , authorURL  = "http://www.iis.sinica.edu.tw/~scm/"
      }
  , Author
      { authorName = "João Saraiva"
      , authorURL  = "http://haslab.uminho.pt/jas/"
      }
  , Author
      { authorName = "Bernhard Westfechtel"
      , authorURL  = "http://www.ai1.uni-bayreuth.de/en/team/Westfechtel_Bernhard/"
      }
  , Author
      { authorName = "Tao Zan"
      , authorURL  = "http://www.prg.nii.ac.jp/members/stefanzan/"
      }
  , Author
      { authorName = "Yongzhe Zhang"
      , authorURL  = "http://zyz915.github.io"
      }
  , Author
      { authorName = "Zirun Zhu"
      , authorURL  = "http://www.prg.nii.ac.jp/members/zhu/"
      }
  ]
