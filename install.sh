set -o errexit
set -o nounset

hash git 2>/dev/null || echo "Git is not installed."; exit
if hash stack 2>/dev/null != 0; then
    wget -qO- https://get.haskellstack.org/ | sh
fi
hash stack 2>/dev/null || echo "Failed to install Haskell Tool stack"; exit
readonly repoUrl="bitbucket.org/5-o/apia"
nc -zw1 "$repoUrl" 443 2>/dev/null || echo "$repoUrl cannot be reached.  Is there an internet connection?"; exit
! [ -e $HOME/.local/bin/apia ] || echo "Apia is already installed."; exit
cd /tmp
trap "rm -rf /tmp/apia; exit" INT TERM EXIT
git clone "$repoUrl" || echo "The clone command failed.  Exiting."; exit
cd apia
stack setup
stack build
if [[ ":$PATH:" != *":$HOME/.local/bin:"* ]]; then
    export PATH=$HOME/.local/bin:$PATH
    echo "export PATH=\$HOME/.local/bin:\$PATH" >> $HOME/.profile
fi
stack install
hash apia 2>/dev/null || echo "Failed to install Apia."; exit
echo "Installation finished successfully."
