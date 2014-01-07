# MAKEFILE PROJET A&G - Malek MAMMAR & Alexandre LE JEAN

EXECUTABLE = main 

all: $(EXECUTABLE)

###########################################################################
#*********************** REGLES GENERIQUES *******************************#
###########################################################################
#%.mli: %.ml
#	ocamlc -i -pp "camlp4o.opt -unsafe" $< > $*.mli

%.cmi: %.mli
	ocamlc -c -pp "camlp4o.opt -unsafe" $<

############################ Parser #######################################

parser.mli: parser.ml myStream.cmo tokenizer.cmo
	ocamlc -i -pp "camlp4o.opt -unsafe" myStream.cmo tokenizer.cmo $< > $@

parser.cmi: parser.mli
	ocamlc -c -pp "camlp4o.opt -unsafe" $<

parser.cmo: parser.ml parser.cmi tokenizer.cmo myStream.cmo
	ocamlc -pp "camlp4o.opt -unsafe" myStream.cmo tokenizer.cmo -c parser.ml


############################ Tokenizer ####################################

tokenizer.mli: tokenizer.ml myStream.cmo
	ocamlc -i -pp "camlp4o.opt -unsafe" myStream.cmo $< > $@

tokenizer.cmi: tokenizer.mli
	ocamlc -c -pp "camlp4o.opt -unsafe" $<

tokenizer.cmo: tokenizer.ml tokenizer.cmi myStream.cmo
	ocamlc -pp "camlp4o.opt -unsafe" myStream.cmo -c tokenizer.ml

############################ MyStream ####################################

myStream.mli: myStream.ml
	ocamlc -i $< > $@

myStream.cmi: myStream.mli
	ocamlc -c $<

myStream.cmo: myStream.ml myStream.cmi 
	ocamlc -c $<

############################ Main ######################################

main: main.ml 
	ocamlbuild -pp "camlp4o.opt -unsafe" main.native	       
	#ocamlc  -pp "camlp4o.opt -unsafe" myStream.cmo tokenizer.cmo parser.cmo -o main

cmo: tokenizer.cmo myStream.cmo parser.cmo

clean:
		rm -fR $(EXECUTABLE) *.cmo *.cmi
		ocamlbuild -clean
