all: day_1 day_2 day_3 day_4

day_1: dec_1/main.hs
	ghc -O dec_1/main.hs -o day_1

day_2: dec_2/main.hs
	ghc -O dec_2/main.hs -o day_2

day_3: dec_3/main.hs
	ghc -O dec_3/main.hs -o day_3

day_4: dec_4/main.hs
	ghc -O dec_4/main.hs -o day_4

bench:
	./day_1 < dec_1/input
	./day_2 < dec_2/input
	./day_3 < dec_3/input
	./day_4 < dec_4/input

clean:
	-rm **/**.hi
	-rm **/**.o
	-rm day_**

.PHONY: clean bench
