all: ../index.html

../index.html: Gen.hs Publications.hs Authors.hs PermVenues.hs
	runghc Gen --publications

# Example usage: make agdahtml dir=WGP11
agdahtml:
	find ../Agda/$(dir)/*.html -exec runghc PatchAgdaHTML.hs {} \;
