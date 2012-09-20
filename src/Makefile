# Config

PKG=mongo
DESTDIR=

# Commands

OCB=ocamlbuild -use-ocamlfind
OCAMLFIND=ocamlfind

#

MAIN_TARGETS = mongo.cmxa mongo.cma mongo.cmi mongo.a
DOC = mongo.docdir/index.html 
AUX_TARGETS = $(DOC)

VPATH=_build

ifeq ($(DESTDIR),)
FIND_FLAGS=
else
FIND_FLAGS=-destdir $(DESTDIR)
endif

all: $(MAIN_TARGETS)

$(MAIN_TARGETS) $(AUX_TARGETS)::
	$(OCB) $@

doc: $(DOC)

install: META $(MAIN_TARGETS) 
	$(OCAMLFIND) install $(FIND_FLAGS) $(PKG) META $(foreach T,$(MAIN_TARGETS),_build/$(T))

uninstall:
	$(OCAMLFIND) remove $(FIND_FLAGS) $(PKG)

reinstall:
	$(MAKE) uninstall
	$(MAKE) install

clean:
	$(OCB) -clean
	@echo
