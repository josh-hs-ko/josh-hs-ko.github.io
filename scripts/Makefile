all: ../index.html

../index.html: GenIndex.hs Publications.hs Authors.hs PermVenues.hs
	runghc GenIndex

agdahtml:
	find ../Agda/$(dir)/*.html -exec stack runghc PatchAgdaHTML.hs {} \;
