# A simple Makefile to automate converting the `*.dot` files into `*.svg`
# files.
.PHONY: all clean

DOTS=$(shell fd -edot)
SVGS=$(patsubst %.dot, %.svg, $(DOTS))

all: $(SVGS)

%.svg: %.dot
	dot -Tsvg $< -o $@

clean:
	-rm $(SVGS)
