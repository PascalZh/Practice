CFLAGS = -std=c++17

all: ch01_sort.out ch01_probabilistic_counting.out ch02_sort.out

ch01_sort.out: ch01_sort.cpp ch01_sort.h test.h utils.h
	g++ $(CFLAGS) $< -o $@

ch01_probabilistic_counting.out: ch01_probabilistic_counting.cpp test.h utils.h
	g++ $(CFLAGS) $< -o $@

ch02_sort.out: ch02_sort.cpp test.h utils.h
	g++ $(CFLAGS) $< -o $@
