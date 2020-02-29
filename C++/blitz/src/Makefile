so = $(shell echo "blitz"`python3-config --extension-suffix`)
CFLAGS = -std=c++11

all: test lib

.PHONY: lib
lib: $(so)
	
core.o: core.cpp core.h
	g++ $(FLAGS) -c core.cpp -o core.o

$(so): pymodule.cpp core.cpp
	g++ $(FLAGS) -shared -fPIC `python3 -m pybind11 --includes` pymodule.cpp core.cpp -o $(so)

test: test.cpp core.o
	g++ $(FLAGS) test.cpp core.o -o test

.PHONY: clean
clean:
	@rm -f *.o
