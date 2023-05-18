# A simple Makefile to automate converting the `*.dot` files into `*.svg`
# files.
# Note that this uses the `dot` command from `graphviz` to do this, so issuing
# ```
# nix-shell -p graphviz
# ```
# prior of running `make all` would probably be a good idea.
.PHONY: all clean

DOTS=$(wildcard *.dot)
SVGS=$(patsubst %.dot, %.svg, $(DOTS))

all: $(SVGS)

%.svg: %.dot
	dot -Tsvg $< -o $@

clean:
	-rm *.svg
