HS_FILES = $(wildcard scripts/*.hs)

all: index.html

index.html: $(HS_FILES)
	runhaskell -iscripts Generate
