#so = $(shell echo "blitz"`python3-config --extension-suffix`)
so = blitz.cpython-36m-x86_64-linux-gnu.so
CFLAGS = -std=c++11

all: test

.PHONY: lib
lib: $(so)

gbk.o: gbk.txt
	objcopy -I binary -O elf64-x86-64 -B i386:x86-64 gbk.txt gbk.o

utils.o: utils.cpp utils.h
	g++ $(FLAGS) -c -o $@ $<

core.o: core.cpp core.h tree.h
	g++ $(FLAGS) -c -o $@ $<

test.o: test.cpp core.h tree.h
	g++ $(FLAGS) -c -o $@ $<

$(so): pymodule.cpp core.cpp utils.cpp utils.h core.h tree.h
	g++ $(FLAGS) -shared -fPIC `python3 -m pybind11 --includes` pymodule.cpp utils.cpp core.cpp -o $(so)

test: test.o core.o utils.o gbk.o
	g++ $(FLAGS) utils.o test.o core.o gbk.o -o test

.PHONY: clean
clean:
	@rm -f *.o
