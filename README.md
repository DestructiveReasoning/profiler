Profiler
========
Profiler is a simple and lightweight ncurses file manager that is written in Haskell. <br />
It uses vim-style keybindings and is modular.

Using Profiler
--------------
Upon launching Profiler, you will be put in **Normal Mode**. <br />
The following keys are used for navigation in **Normal Mode**:
```
j,k:  Move down/up
h:    Move up a directory
l:    If a directory is selected, navigate to that directory. Else execute the selected file.
o:    Prompts for name of program to open the file with
g:    Navigate to the first file in the directory
G:    Navigate to the last file in the directory
^:    Navigate to the user's home directory
/:    Search current directory
n:    Next search result
Tab:  Switch frames
q:    Quit Profiler
```

*Profiler is still in development. More keybindings and features are coming.*

Configuration
-------------
### Default Applications
Default applications can be specified in the `~/.profiler` file, with the following syntax:
```
<file extension 1>, <program 1>
<file extension 2>, <program 2>
<file extension 3>, <program 3>
...
```
For example, the following is a valid `~/.profiler` file:
```
pdf, evince
mp4, vlc
png, feh
doc, libreoffice
xlsx, libreoffice
```
Opening files with CLI/TUI programs, such as Vim, can be quite buggy when not done properly.<br>
For such cases, it is recommended to open the file with your CLI/TUI program in *another* terminal emulator instance, as follows:<br>
*~/.profiler*
```
hs, xterm -e vim
```

Dependencies
------------
Profiler depends on:
* ncurses
* The HSCurses library (`# cabal install hscurses`) *(for building only)*
* `Data.List.Split` installed in Gentoo via (`# emerge dev-haskell/split`) *(for building only)*
* Haskell's `regex-base`, installed in Gentoo via `# emerge dev-haskell/regex-compat` *(for building only)*
* The Glorious Glasgow Haskell Compiler (GHC) version 8.0.2 or newer *(for building only)*

Installing Profiler
-------------------
To install Profiler, navigate to the directory of your choice and run the following commands:
```
$ git clone https://github.com/DestructiveReasoning/profiler.git
$ cd profiler
# make install
```
Profiler can then be launched: `$ profiler`.

Building Profiler
-----------------
To build Profiler, ensure that you have the dependencies outlined above, and run the following in the directory of your choice:
```
$ git clone https://github.com/DestructiveReasoning/profiler.git
$ cd profiler
$ make
```
