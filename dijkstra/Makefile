CC			= ghc
SOURCES		= $(shell find $(SOURCEDIR) -maxdepth 1 -name '*.hs')
FLAGS		= --make
OBJS		= $(SOURCES:.hs=) 
MAKEFLAGS   = --no-print-directory

%:
	$(CC) $(FLAGS) $@

all:
	@make --no-print-directory clean
	@make --no-print-directory $(OBJS);

clean:
	rm -rf $(OBJS) *.o *.hi
