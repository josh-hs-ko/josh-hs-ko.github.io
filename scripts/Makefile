all: ../index.html

../index.html: Gen.hs Publications.hs Authors.hs PermVenues.hs
	runghc Gen --publications

agdahtml:
	find ../Agda/$(dir)/*.html -exec stack runghc PatchAgdaHTML.hs {} \;
