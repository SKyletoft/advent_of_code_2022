all: day_1 day_2 day_3 day_4 day_5 day_6 day_7 day_8 day_9 day_10 day_11 day_12 day_13 day_14 day_15

day_1: dec_1/main.hs
	ghc -O $^ -o $@

day_2: dec_2/main.hs
	ghc -O $^ -o $@

day_3: dec_3/main.hs
	ghc -O $^ -o $@

day_4: dec_4/main.hs
	ghc -O $^ -o $@

day_5: dec_5/main.hs
	ghc -O $^ -o $@

day_6: dec_6/main.hs
	ghc -O $^ -o $@

day_7: dec_7/main.hs
	ghc -O $^ -o $@

day_8: dec_8/main.hs
	ghc -O $^ -o $@

day_9: dec_9/main.hs
	ghc -O $^ -o $@

day_10: dec_10/main.hs
	ghc -O $^ -o $@

day_11: dec_11/main.hs
	ghc -O $^ -o $@

day_12: dec_12/main.hs
	ghc -O $^ -o $@

day_13: dec_13/main.hs
	ghc -O $^ -o $@

day_14: dec_14/main.hs
	ghc -O $^ -o $@

day_15: dec_15/main.hs
	ghc -O $^ -o $@

day_16: dec_16/main.hs
	ghc -O $^ -o $@

day_17: dec_17/main.hs
	ghc -O $^ -o $@

day_18: dec_18/main.hs
	ghc -O $^ -o $@

day_19: dec_19/main.hs
	ghc -O $^ -o $@

day_20: dec_20/main.hs
	ghc -O $^ -o $@

day_21: dec_21/main.hs
	ghc -O $^ -o $@

day_22: dec_22/main.hs
	ghc -O $^ -o $@

day_23: dec_23/main.hs
	ghc -O $^ -o $@

day_24: dec_24/main.hs
	ghc -O $^ -o $@

day_25: dec_25/main.hs
	ghc -O $^ -o $@

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
