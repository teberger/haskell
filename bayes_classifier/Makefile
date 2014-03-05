default: clean

clean:
	rm *.hi *.o *~

compile: 
	ghc --make bayesClassifier.hs -o classifier

run: compile
	./classifer vocabulary 20news-bydate/matlab/

edit:
	emacs -nw bayesClassifier.hs
