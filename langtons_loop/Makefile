default: clean

clean: 
	rm *.hi *.o langtons *~

compile:
	ghc --make langtons.hs -o langtons

edit:
	emacs -nw langtons.hs


run: compile
	./langtons
