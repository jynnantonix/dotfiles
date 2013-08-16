# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=500
SAVEHIST=500
setopt extendedglob
setopt null_glob
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/chirantan/.zshrc'

autoload -U compinit
compinit
# End of lines added by compinstall

# modified commands
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias ls='ls -hF --color=auto'
alias la='ls -a'
alias ll='ls -l'
alias lal='ls -al'
alias grep='grep --color=auto'
alias hgrep='history | grep '
alias psgrep='ps aux | grep'
alias df='df -h'
alias du='du -c -h -s'
alias mkdir='mkdir -p -v'
alias ping='ping -c 5'
alias dstat='dstat -cdnpmgs --top-bio --top-cpu'
alias iftop='sudo iftop -B -i wlp3s0'

# sudo commands
if [ $UID -ne 0 ]; then
    alias sudo='sudo '
    alias scat='sudo cat'
    alias snano='sudo nano'
    alias semacs='sudo emacs'
fi

# safety features
alias cp='cp -i'
alias mv='mv -i'
alias ln='ln -i'
alias rm='rm -I'                  # -i prompts for every file
alias chown='chown --preserve-root'
alias chmod='chmod --preserve-root'
alias chgrp='chgrp --preserve-root'

# screen locking because for some reason it doesn't work automagically
alias slock='gnome-screensaver-command -l'

# take advantage of the 32 cores
alias make='make -j40'

# set the correct umask for cros buiding
umask 022

# add depot_tools to path
export PATH="$PATH:/usr/local/google/home/ekbotec/Projects/depot_tools"

# add my bin scripts to path
export PATH="/usr/local/google/home/ekbotec/bin:$PATH"

# connect to an already running ssh agent
ssh-reagent() {
    for agent in /tmp/ssh-*/agent.*; do
        export SSH_AUTH_SOCK="${agent}";
	if [[ -n $(ssh-add -l 2>/dev/null) ]]; then
            echo "Found working SSH Agent" ;
            ssh-add -l ;
            return
        fi
    done
    echo "Cannot find working SSH Agent. Creating new agent";
    eval `ssh-agent`;
}

# Configure the prompt
autoload -U colors
colors
autoload -U promptinit
promptinit

prompt_status() {
    RETVAL=$?
    if [ $RETVAL -ne 0 ]; then
	echo -n ":( $RETVAL"
    else
	echo -n ":) $RETVAL"
    fi
}

PROMPT="%{$fg[white]%}┌─[%{$fg[yellow]%}%*%{$fg[white]%}][%{$fg[magenta]%}%m%{$fg[white]%}][%{$fg[cyan]%}%n%{$fg[white]%}][%{$fg[green]%}%~%{$fg[white]%}]
└───╼ "
RPROMPT="[%?]"
