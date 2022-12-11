all: day_1 day_2 day_3 day_4 day_5 day_6 day_7 day_8 day_9 day_10 day_11

day_1: dec_1/main.hs
	ghc -O dec_1/main.hs -o day_1

day_2: dec_2/main.hs
	ghc -O dec_2/main.hs -o day_2

day_3: dec_3/main.hs
	ghc -O dec_3/main.hs -o day_3

day_4: dec_4/main.hs
	ghc -O dec_4/main.hs -o day_4

day_5: dec_5/main.hs
	ghc -O dec_5/main.hs -o day_5

day_6: dec_6/main.hs
	ghc -O dec_6/main.hs -o day_6

day_7: dec_7/main.hs
	ghc -O dec_7/main.hs -o day_7

day_8: dec_8/main.hs
	ghc -O dec_8/main.hs -o day_8

day_9: dec_9/main.hs
	ghc -O dec_9/main.hs -o day_9

day_8: dec_10/main.hs
	ghc -O dec_10/main.hs -o day_10

day_8: dec_11/main.hs
	ghc -O dec_11/main.hs -o day_11

bench: all
	hyperfine -w20 "\
		./day_1  <  dec_1/input &&\
		./day_2  <  dec_2/input &&\
		./day_3  <  dec_3/input &&\
		./day_4  <  dec_4/input &&\
		./day_5  <  dec_5/input &&\
		./day_6  <  dec_6/input &&\
		./day_7  <  dec_7/input &&\
		./day_8  <  dec_8/input &&\
		./day_9  <  dec_9/input &&\
		./day_10 < dec_10/input &&\
		./day_11 < dec_11/input"

clean:
	-rm **/**.hi
	-rm **/**.o
	-rm day_**

.PHONY: clean bench
