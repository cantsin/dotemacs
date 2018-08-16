#!/bin/bash

RCFILE=~/.zshrc

if [[ ! -e ~/.cask ]]
then
    echo "Cloning Cask repo"
    git clone https://github.com/cask/cask.git ~/.cask
fi

if [[ $(grep "cask/bin" ~/.bash_profile) == "" ]]
then
    echo "Adding \$HOME/.cask/bin to \$PATH in \$RCFILE"
    echo '' >> $RCFILE
    echo "# Added by ~/.emacs.d/install.sh" >> $RCFILE
    echo "export PATH=\$HOME/.cask/bin:\$PATH" >> $RCFILE
fi

export PATH=$HOME/.cask/bin:$PATH

cd ~/.emacs.d
cask install
