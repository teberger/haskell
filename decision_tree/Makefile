default: clean

clean : 
	rm *~ *.o *.hi

compile : 
	ghc --make constructTree.hs -o decisionTree

run : compile
	./decisionTree training.txt validation.txt

edit : 
	emacs -nw constructTree.hs
