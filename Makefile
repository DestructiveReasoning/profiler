builddir=build/
bindir=bin/
srcdir=src/
configdir=config/
legacysrcdir=$(srcdir)Legacy/
profilerdir=~/.profiler

Profiler: build clean

init:
	mkdir -p $(builddir)
	mkdir -p $(bindir)
	mkdir -p $(profilerdir)

build: init
	ghc -dynamic $(srcdir)Main.hs $(srcdir)CommanderGeneral.hs $(srcdir)Dispatch.hs $(srcdir)Interrogator.hs $(srcdir)ColorManager.hs -odir $(builddir) -hidir $(builddir) -o $(bindir)profiler

legacy: init
	ghc $(legacysrcdir)Main.hs $(legacysrcdir)CommanderGeneral.hs -odir $(builddir) -hidir $(builddir) -o $(bindir)profiler

clean:
	rm -rf $(builddir)

install: installconfig
	install -Dm755 $(bindir)profiler /bin/profiler

installconfig:
	mkdir -p $(profilerdir)
	rsync -av --ignore-existing $(configdir) $(profilerdir)
