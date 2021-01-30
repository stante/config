#                    ██                    
#                   ░██                    
#     ██████  ██████░██      ██████  █████ 
#    ░░░░██  ██░░░░ ░██████ ░░██░░█ ██░░░██
#       ██  ░░█████ ░██░░░██ ░██ ░ ░██  ░░ 
#  ██  ██    ░░░░░██░██  ░██ ░██   ░██   ██
# ░██ ██████ ██████ ░██  ░██░███   ░░█████ 
# ░░ ░░░░░░ ░░░░░░  ░░   ░░ ░░░     ░░░░░  
# Alexander Stante's .zshrc file

# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory autocd extendedglob nomatch notify
unsetopt beep
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/alex/.zshrc'

autoload -Uz compinit
zstyle ':completion:*' menu select
compinit
# End of lines added by compinstall

alias ls='exa'
alias ll='exa -la'
alias diff='diff --color'
alias tree='tree -AC'
PS1='[%~]$ '

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/alex/opt/miniconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/alex/opt/miniconda3/etc/profile.d/conda.sh" ]; then
        . "/home/alex/opt/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="/home/alex/opt/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<
