# TextScape

## Requirements

To run the TextScape interpreter you must have the Haskell Platform installed.

To run the server and shell ruby scripts ruby is required.

To view the served editor, a web browser supporting JavaScript is required.

## Usage

### Interpreter

To run a TextScape interpreter session, from the code folder type:

```
cabal run
```

This is a machine friendly interpreter session however, and is not very 
user friendly. For a more user friendly experience, you can try:

```
cabal repl
startShell
```

This will open a shell session inside a Haskell interpreter REPL. However,
there are not many creature comforts in this shell. If you have ruby installed,
try running the shell.rb script, this will provide you with fun things such
as readline support and some color.

### Web Interface

To get started with the web interface, you must have Ruby installed. From
the project directory run the server.rb, next point your web browser at
localhost:1234. I know the port should be changable, but hey, its the
quarter system.

## Notes

test.ts is a TextScape source file used for testing. stdlib.ts is loaded
by the interpreter every time it is started.
