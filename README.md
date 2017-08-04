Profiler
========
Profiler is a simple and lightweight ncurses file manager that is written in Haskell. <br />
It uses vim-style keybindings and is modular.

Using Profiler
--------------
Upon launching Profiler, you will be put in **Normal Mode**. <br />
The following keys are used for navigation in **Normal Mode**:
* **j,k**:		Move down/up
* **h**:		Move up a directory
* **l**:		If a directory is selected, navigate to that directory. Else execute the selected file.
* **g**:		Navigate to the first file in the directory
* **G**:		Navigate to the last file in the directory
* **^**:		Navigate to the user's home directory
* **Tab**:		Switch frames

*Profiler is still in development. More keybindings and features are coming.*

Dependencies
------------
Profiler depends on:
* ncurses
* The HSCurses library (`# cabal install hscurses`)
* Haskell's `regex-base`, which was installed in Gentoo via `# emerge dev-haskell/regex-compat`

Installing Profiler
-------------------
To install profiler, navigate to the directory of your choice and run the following commands:
```
$ git clone https://github.com/DestructiveReasoning/profiler.git
$ cd profiler
# make install
```
Profiler can then be launched: `$ profiler`.

Building Profiler
-----------------
To build profiler, ensure that you have the dependencies outlined above, and run the following in the directory of your choice:
```
$ git clone https://github.com/DestructiveReasoning/profiler.git
$ cd profiler
$ make
```
