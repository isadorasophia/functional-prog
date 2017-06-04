CC			= ghc
SOURCES		= $(shell find $(SOURCEDIR) -maxdepth 1 -name '*.hs')
OBJS		= $(SOURCES:.hs=) 
MAKEFLAGS   = --no-print-directory

%.hs:
	$(CC) $@.c -o $@

all:
	@make --no-print-directory clean
	@make --no-print-directory $(OBJS);

clean:
	rm -rf $(OBJS)
