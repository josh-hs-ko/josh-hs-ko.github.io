module PermVenues where


data PermVenue = PermVenue
  { permVenueName :: String
  , permVenueURL  :: String
  }


permVenueList :: [PermVenue]
permVenueList =
  [ PermVenue
      { permVenueName = "arXiv"
      , permVenueURL  = "https://arxiv.org"
      }
  , PermVenue
      { permVenueName = "GRACE Center, National Institute of Informatics"
      , permVenueURL  = "http://grace-center.jp"
      }
  , PermVenue
      { permVenueName = "Journal of Functional Programming"
      , permVenueURL  = "https://www.cambridge.org/core/journals/journal-of-functional-programming"
      }
  , PermVenue
      { permVenueName = "Progress in Informatics"
      , permVenueURL  = "https://www.nii.ac.jp/pi/"
      }
  , PermVenue
      { permVenueName = "University of Oxford"
      , permVenueURL  = "http://www.ox.ac.uk"
      }
  ]
