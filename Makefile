builddir="build/"
bindir="bin/"
srcdir="src/"

Profiler: build clean

init:
	mkdir -p $(builddir)
	mkdir -p $(bindir)

build: init
	ghc $(srcdir)Main.hs $(srcdir)CommanderGeneral.hs $(srcdir)Dispatch.hs -odir $(builddir) -hidir $(builddir) -o $(bindir)profiler

clean:
	rm -rf $(builddir)

install:
	install -Dm755 $(bindir)profiler /bin/profiler
