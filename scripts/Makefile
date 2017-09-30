all: ../index.html

../index.html: Generate.hs Publications.hs Authors.hs PermVenues.hs
	runhaskell Generate

agdahtml:
	find ../Agda/$(dir)/*.html -exec runghc PatchAgdaHTML.hs {} \;
