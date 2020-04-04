#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# set maximum lines in history file
HISTFILESIZE=5000000
HISTSIZE=5000000

# Avoid duplicates
HISTCONTROL=ignoreboth:erasedups
# Avoid cd and rm in history
HISTIGNORE='cd *:rm *'
# When the shell exits, append to the history file instead of overwriting it
shopt -s histappend

# After each command, append to the history file and reread it
PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND$'\n'}history -a; history -c; history -r"

man() {
    LESS_TERMCAP_md=$'\e[01;31m' \
    LESS_TERMCAP_me=$'\e[0m' \
    LESS_TERMCAP_se=$'\e[0m' \
    LESS_TERMCAP_so=$'\e[01;44;33m' \
    LESS_TERMCAP_ue=$'\e[0m' \
    LESS_TERMCAP_us=$'\e[01;32m' \
    command man "$@"
}

# Add ruby gem path to PATH variable
PATH="$PATH:$(ruby -e 'puts Gem.user_dir')/bin"
PATH="$PATH:~/.npm-global/bin"

alias ls='ls --color=auto'
alias ll='ls --color=auto -lah'
alias gst='git status'
alias pbcopy='xsel --clipboard --input'
alias pbpaste='xsel --clipboard --output'
alias update='sudo pacman -Syu'
PS1='[\u@\h \W]\$ '
