set -o errexit
set -o nounset

itWentWrong() {
    echo $1
    exit
}

hash git 2>/dev/null || itWentWrong "Git is not installed."

readonly repoUrl="https://bitbucket.org/5-o/apia.git"
! nc -zw1 "$repoUrl" 443 2>/dev/null || itWentWrong "Can't connect to $repoUrl"

installStack() {
    wget -qO- https://get.haskellstack.org/ | sh
}

hash stack 2>/dev/null || installStack

hash stack 2>/dev/null || itWentWrong "Stack is not installed."


! [ -e $HOME/.local/bin/apia ] || itWentWrong "Apia is already installed."
cd $HOME
trap "rm -rf /tmp/apia; exit" INT TERM EXIT
git clone "$repoUrl" || itWentWrong "The clone command failed."
cd apia
stack setup
stack build
if [[ ":$PATH:" != *":$HOME/.local/bin:"* ]]; then
    export PATH=$HOME/.local/bin:$PATH
    echo "export PATH=\$HOME/.local/bin:\$PATH" >> $HOME/.profile
fi
stack install
hash apia 2>/dev/null || itWentWrong "Failed to install Apia."
echo "Installation finished successfully."
