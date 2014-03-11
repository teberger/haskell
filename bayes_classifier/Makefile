default: clean

clean:
	rm *.hi *.o *~ classifier

compile: 
	ghc --make bayesClassifier.hs -o classifier -O2

run: compile
	./classifier vocabulary.txt newsgrouplabels.txt data/ -N

edit:
	emacs -nw bayesClassifier.hs
