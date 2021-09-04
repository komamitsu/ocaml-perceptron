.PHONY: run

perceptron.cmo:
	ocamlc -c perceptron.ml

run: perceptron.cmo
	ocaml perceptron.cmo sample.ml
