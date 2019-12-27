test_objects := build/test.o build/AGo.o
test_c := test/test.cpp AGo.cpp
#LDFLAGS := $(shell python3.5-config --ldflags)
#CFLAGS := $(shell python3.5-config --cflags)

CFLAGS += -std=c++11
#-I$(shell echo $$BOOST_INCLUDE_PATH)
LDFLAGS += -lfmt -lpthread -lboost_filesystem -lboost_system

$(shell if [ ! -d build ]; then mkdir build; fi)

Test : $(test_objects)
	g++ -o Test $(test_objects) $(LDFLAGS)

build/test.o : test/test.cpp AGo.h
	g++ -o build/test.o -c test/test.cpp $(CFLAGS)
build/AGo.o : AGo.cpp AGo.h
	g++ -o build/AGo.o -c AGo.cpp $(CFLAGS)
.PHONY : clean
clean :
	rm Test $(test_objects)
