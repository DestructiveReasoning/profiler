Profiler
========
Profiler is a simple and lightweight ncurses file manager that is written in Haskell. <br />
It uses vim-style keybindings and is modular.

Using Profiler
--------------
Upon launching Profiler, you will be put in **Normal Mode**. <br />
The following keys are used for navigation in **Normal Mode**:

Key | Action
-------: | :------
j,k | Move down/up
h | Move up a directory
l | If a directory is selected, navigate to that directory. Else execute the selected file.
o | Prompts for name of program to open the file with
g | Navigate to the first file in the directory
G | Navigate to the last file in the directory
d | Delete the selected file
y | Copy the selected file to a destination defined by an input prompt
S | Move the selected file to a destination defined by an input prompt
a | Create a directory (and its parents) defined by an input prompt
^ | Navigate to the user's home directory
/ | Search current directory
n | Next search result
N | Previous search result
Tab | Switch frames
q | Quit Profiler

*Profiler is still in development. More keybindings and features are coming.*

Configuration
-------------
### Default Applications
Default applications can be specified in the `~/.profiler/defaults` file, with the following syntax:
```
<file extension 1>, <program 1>
<file extension 2>, <program 2>
<file extension 3>, <program 3>
...
```
For example, the following is a valid `~/.profiler/defaults` file:
```
pdf, evince
mp4, vlc
png, feh
doc, libreoffice
xlsx, libreoffice
```
Opening files with CLI/TUI programs, such as Vim, can be quite buggy when not done properly.<br>
For such cases, it is recommended to open the file with your CLI/TUI program in *another* terminal emulator instance, as follows:<br>
*~/.profiler/defaults*
```
hs, xterm -e vim
```
When trying to open a file type whose extension is not included in the `~/.profiler/defaults` file, Profiler will fall back to `xdg-open`.

### Custom Colorscheme
Custom color settings can be defined in the `~/.profiler/colors` file, with the following syntax:
```
<color class1> <foreground1> <background1>
<color class2> <foreground2> <background2>
```
Foreground and background colors are 8-bit, so should be written as a number between 0 and 255, or -1 for the default terminal foreground or background.<br>
Color class defines which part of the UI the following color pair should be used for.<br>
Currently, the valid color classes are:
* selected: Color of the selected item
* folder: Color used for folders
* executable: Color used for executable files
* border: Color used for the border of the selected pane
* cwd: Color used for the output of the current working directory
* error: Color used for error messages
* symlink: Color used for a file that is a symbolic link
Note that if a `~/.profiler/colors` exists, it is recommended that all color classes be defined. Otherwise the colorscheme may look ugly.
The default colorscheme would look like:
```
selected 4 -1
folder 4 -1
executable 83 -1
border 220 -1
cwd 220 -1
error 160 -1 
symlink 63 -1
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
