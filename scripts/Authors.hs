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
      { authorName = "Yun-Sheng Chang"
      , authorURL  = "https://yunshengtw.github.io"
      }
  , Author
      { authorName = "Liang-Ting Chen"
      , authorURL  = "https://l-tchen.github.io"
      }
  , Author
      { authorName = "Yu-Fang Chen"
      , authorURL  = "http://bull.iis.sinica.edu.tw/yfc/doku.php"
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
      { authorName = "Liye Guo"
      , authorURL  = "https://ieeexplore.ieee.org/author/37086808544"
      }
  , Author
      { authorName = "Zhenjiang Hu"
      , authorURL  = "https://zhenjiang888.github.io"
      }
  , Author
      { authorName = "Keigo Imai"
      , authorURL  = "http://www.ct.info.gifu-u.ac.jp/~keigoi/"
      }
  , Author
      { authorName = "Patrik Jansson"
      , authorURL  = "http://www.cse.chalmers.se/~patrikj/"
      }
  , Author
      { authorName = "Tzu-Chi Lin"
      , authorURL  = "https://github.com/Zekt"
      }
  , Author
    { authorName = "Pedro Martins"
    , authorURL  = "https://dblp.org/pid/59/2784-1.html"
    }
  , Author
      { authorName = "Jorge Mendes"
      , authorURL  = "http://wiki.di.uminho.pt/twiki/bin/view/Personal/JorgeMendes/WebHome"
      }
  , Author
      { authorName = "Shin-Cheng Mu"
      , authorURL  = "https://scm.iis.sinica.edu.tw/home/"
      }
  , Author
      { authorName = "João Saraiva"
      , authorURL  = "http://haslab.uminho.pt/jas/"
      }
  , Author
      { authorName = "Zhixuan Yang"
      , authorURL  = "https://yangzhixuan.github.io"
      }
  , Author
      { authorName = "Nobuko Yoshida"
      , authorURL  = "http://mrg.doc.ic.ac.uk/people/nobuko-yoshida/"
      }
  , Author
      { authorName = "Tao Zan"
      , authorURL  = "http://www.prg.nii.ac.jp/members/stefanzan/"
      }
  , Author
      { authorName = "Yongzhe Zhang"
      , authorURL  = "https://zyz915.github.io"
      }
  , Author
      { authorName = "Zirun Zhu"
      , authorURL  = "https://ac.k331.one"
      }
  ]
