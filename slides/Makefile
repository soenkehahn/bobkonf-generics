all: slides.pdf check ack

ack:
	! ack-grep todo *.lhs

clean:
	rm -rf slides.pdf slides.pdf

slides.pdf: slides.lhs relations.pdf HasEot.pdf
	pandoc slides.lhs -f markdown+lhs -o slides.pdf -s -t beamer

check: .cabal-sandbox
	doctest slides.lhs
	doctest getopt-generics.hs

.cabal-sandbox: tinc.yaml
	tinc

%.pdf : %.dot
	dot -Tpdf $< -o $@
	# convert relations.dot.pdf -scale 100 relations.dot.pdf
