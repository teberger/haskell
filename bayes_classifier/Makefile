default: clean

clean:
	rm *.hi *.o *~ classifier

compile: 
	ghc --make bayesClassifier.hs -o classifier

run: compile
	./classifier vocabulary.txt data/

edit:
	emacs -nw bayesClassifier.hs
