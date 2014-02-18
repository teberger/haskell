default: clean

clean : 
	rm *.o *.hi *.aux *.log writeup.synctex.gz

compile : 
	ghc --make constructTree.hs -o decisionTree

run : compile
	./decisionTree training.txt validation.txt

edit : 
	emacs -nw constructTree.hs
