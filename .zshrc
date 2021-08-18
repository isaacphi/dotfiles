setopt histignorealldups sharehistory

# Use emacs keybindings even if our EDITOR is set to vi
bindkey -e

# disable matches which are usually just annoying
setopt -o nomatch

# Keep 5000 lines of history within the shell and save it to ~/.zsh_history:
HISTSIZE=5000
SAVEHIST=5000
HISTFILE=~/.zsh_history

# Use modern completion system
autoload -Uz compinit
compinit

export EDITOR="emacs -nw"
export PATH=~/src/todo:$PATH

alias t="todo.sh -d /home/phil/.todo.cfg"
alias nord="nordvpn"

# node
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
fpath=($fpath "/home/$USER/.zfunctions")

# android studio and flutter
export PATH="/home/$USER/src/android-studio/bin:$PATH"
export PATH="$PATH":"$HOME/.pub-cache/bin"

# pyenv
export PATH="/home/$USER/.pyenv/bin:$PATH"
export PATH="/home/$USER/.pyenv/shims:$PATH"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

autoload -Uz compinit; compinit

export PATH="/home/$USER/src/:$PATH"
fpath=($fpath "/home/$USER/.zfunctions")

# prompt
PROMPT='%(?.%F{green}☉.%F{red}?%?)%f %B%F{240}%2~%f%b %# '
# git prompt
autoload -Uz vcs_info
precmd_vcs_info() { vcs_info }
precmd_functions+=( precmd_vcs_info )
setopt prompt_subst
RPROMPT=\$vcs_info_msg_0_
zstyle ':vcs_info:git:*' formats '%F{240}(%b)%r%f'
zstyle ':vcs_info:*' enable git
