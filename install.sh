#!/bin/sh

# ---------------------------------------------
# It installs Apia, a command-line time logger.
# ---------------------------------------------

# It has the effect that any line with a non-zero error code will
# stop the script.
set -o errexit

# It prevents undeclared variables.
set -o nounset

itWentWrong() {
    echo "$1"
    exit
}

# Check that git is installed and exit if not.
hash git 2>/dev/null || itWentWrong "Git is not installed."

# Check there is an internet connection to the Apia repository and 
# quit if not.
readonly repoUrl="https://bitbucket.org/5-o/apia.git"
! nc -zw1 "$repoUrl" 443 2>/dev/null || \
    itWentWrong "Can't connect to $repoUrl"

installStack() {
    wget -qO- https://get.haskellstack.org/ | sh
}

# Install the Haskell Tool stack if it isn't there already.
hash stack 2>/dev/null || installStack

# Check that the Haskell Tool Stack is installed and quit if not.
hash stack 2>/dev/null || itWentWrong "Stack is not installed."

# Download the code and compile it.
cd /tmp
trap "rm -rf /tmp/apia; exit" INT TERM EXIT
git clone "$repoUrl" || itWentWrong "The clone command failed."
cd apia
stack setup
stack build

readonly bindir="$HOME/.local/bin"

add2path() {
    PATH="$bindir":"$PATH"
    # Makes the change to PATH variable permanent by putting
    # it in the ~/.profile file.
    echo "export PATH=\$HOME/.local/bin:\$PATH" >> "$HOME"/.profile
}

# Add the directory ~/.local/bin to the PATH variable if not already
# there.
case :$PATH: in
  *:bindir:*)  ;;  # do nothing
  *) add2path ;;
esac

# Puts the binary in ~/.local/bin
stack install

# Checks that Apia is installed and quits if not.
hash apia 2>/dev/null || itWentWrong "Failed to install Apia."

echo "Installation finished successfully."
