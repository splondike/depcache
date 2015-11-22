The program processes revision controlled directories looking for dependency management files (e.g. composer, bower, npm) and downloads the latest dependencies. The idea is to use this to make a complete working backup of your software in case external dependencies are removed.

# Building

The program is written in Haskell and uses the Stackage system. Follow [the instructions](https://github.com/commercialhaskell/stack#how-to-install) to install that.

After that, you can build the executable using `stack build`. Near the end of the output, the program will say "Installing executable(s) in <path>". Look in path for the depcache executable.

# Running

Run the progam without arguments and it should print a usage string telling you what arguments it needs, and what its return codes are.
