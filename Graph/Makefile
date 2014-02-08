objectFiles=$(find -name *.o)
hiFiles=$(find -name *.hi)

default: clean

clean:
	rm *.o *.hi ./Algorithms/*.o ./Algorithms/*.hi ./Implementations/*.o ./Implementations/*.hi

compile: 
	ghc *.hs ./Algorithms/*.hs ./Implementations/*.hs

edit:
	emacs *.hs -nw
