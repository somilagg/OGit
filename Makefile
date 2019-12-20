MODULES=test diff command main storage users authors
OBJECTS=$(MODULES:=.cmo)
MLIS= diff.mli main.mli storage.mli users.mli authors.mli
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS) 

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

docs: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal,diff \
		-html -stars -d doc.public -hide-warnings $(MLIS)

clean:
	ocamlbuild -clean
	rm -rf doc.public