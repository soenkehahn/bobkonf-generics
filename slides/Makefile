all: slides.pdf check

ack:
	! ack-grep -H todo *.lhs

clean:
	rm -rf *.pdf

slides.pdf: slides.lhs relations.pdf HasEot.pdf
	cat slides.lhs | sed -r 's/\$$ >>>/>/' | sed -r 's/>>>/>/' | \
		pandoc - -f markdown+lhs -o slides.pdf -s -t beamer

check: .cabal-sandbox
	doctest slides.lhs

.cabal-sandbox: tinc.yaml
	tinc

%.pdf : %.dot
	dot -Tpdf $< -o $@
	# convert relations.dot.pdf -scale 100 relations.dot.pdf
