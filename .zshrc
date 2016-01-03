# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory autocd extendedglob
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/fabio/.zshrc'

autoload -Uz compinit promptinit
compinit
promptinit
# End of lines added by compinstall
prompt walters

# if a new command line added duplicates an old one, the older
# command is removed
setopt HIST_IGNORE_ALL_DUPS
# when writing the history file, older duplicates are omitted
setopt HIST_SAVE_NO_DUPS

setopt INC_APPEND_HISTORY

# colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    #alias fgrep='fgrep --color=auto'
    #alias egrep='egrep --color=auto'
fi
# some more ls aliases
alias ll='ls -lh'
alias la='ls -lah'
alias cat='cat -v'
alias less='less -R'
alias vim='gvim'
alias gst='git status'
alias svs='svn status'
alias picorun='sudo picocom -b 115200 /dev/ttyUSB0'
alias nssh='ssh -o "StrictHostKeyChecking no" -o "UserKnownHostsFile=/dev/null"'
alias nscp='scp -o "StrictHostKeyChecking no" -o "UserKnownHostsFile=/dev/null"'
alias pmutt='mutt -F .mutt/muttrc-local'

export PATH=$PATH:/home/fabio/bin
export WORKON_HOME=~/src/virtualenvs
source /usr/local/bin/virtualenvwrapper.sh
TERM="gnome-256color"
# autologin
[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && exec startx
# Hook for desk activation
[ -n "$DESK_ENV" ] && source "$DESK_ENV"
