builddir="build/"
bindir="bin/"

Profiler: init
	ghc src/Main.hs src/CommanderGeneral.hs -odir $(builddir) -o $(bindir)profiler

init:
	mkdir -p $(builddir)
	mkdir -p $(bindir)

clean:
	rm -rf $(builddir)

install:
	install -Dm755 $(bindir)profiler /bin/profiler
