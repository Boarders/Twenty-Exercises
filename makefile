dir = src
src = $(wildcard $(dir)/*.hs)

all: $(src)
	ghc -fno-code $^
