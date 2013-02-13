TARGET=tvon

CC=gcc
CFLAGS=-g -O2 $(shell curl-config --cflags) -Wall -m32 -mmacosx-version-min=10.6
LDFLAGS=-a -lpthread -lm $(shell curl-config --libs) -framework foundation -framework AppKit $(CFLAGS)
LD=gcc
UNAME=$(shell uname)

SOURCES =	\
    main.m	\

ifndef DESTDIR
    DESTDIR := /usr
endif

OBJECTS:=$(SOURCES:%.c=build/%.o)
OBJECTS:=$(OBJECTS:%.m=build/%.o)

default : $(TARGET)

build/%.o : %.m build
	$(CC) $(CFLAGS) -c -o $@ $<
	$(CC) -MM $(CLFAGS) $< > build/$*.d

build/%.o : %.m build
	$(CC) $(CFLAGS) -c -o $@ $<
	$(CC) -MM $(CLFAGS) $< > build/$*.d

$(TARGET): $(OBJECTS)
	$(LD) -o $(TARGET) $(OBJECTS) $(LDFLAGS)

.PHONY : clean
clean: 
	rm -Rf build/*
	rm -f $(TARGET)

build:
	@mkdir -p build

install: $(TARGET)
	install -m 0755 $(TARGET) $(DESTDIR)/bin/$(TARGET)

