all: index.html

index.html: scripts/Generate.hs scripts/Publications.hs scripts/Authors.hs scripts/PermVenues.hs
	runhaskell -iscripts Generate

agdahtml:
	find Agda/$(dir)/*.html -exec runghc scripts/PatchAgdaHTML.hs {} \;
