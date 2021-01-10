setopt histignorealldups sharehistory

# Use emacs keybindings even if our EDITOR is set to vi
bindkey -e

# Keep 1000 lines of history within the shell and save it to ~/.zsh_history:
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.zsh_history

# Use modern completion system
autoload -Uz compinit
compinit

export EDITOR="emacs -nw"
export PATH=~/src/todo:$PATH

alias t="todo.sh -d /home/phil/.todo.cfg"
alias nord="nordvpn"

# plugins=(virtualenv)

# ZSH_THEME=robbyrussell

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
fpath=($fpath "/home/$USER/.zfunctions")

# Set Spaceship ZSH as a prompt
autoload -U promptinit; promptinit
prompt spaceship

eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

export PATH="/home/$USER/src/:$PATH"
export PATH="/home/$USER/.pyenv/bin:$PATH"
fpath=($fpath "/home/$USER/.zfunctions")

