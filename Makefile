builddir="build/"
bindir="bin/"
srcdir="src/"

Profiler: build clean

init:
	mkdir -p $(builddir)
	mkdir -p $(bindir)

build: init
	ghc $(srcdir)Main.hs $(srcdir)CommanderGeneral.hs -odir $(builddir) -o $(bindir)profiler

clean:
	rm -rf $(builddir)
	rm $(srcdir)*.hi

install:
	install -Dm755 $(bindir)profiler /bin/profiler
