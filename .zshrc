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

# get a random saying for the day
command fortune | cowsay -$(shuf -n 1 -e b d g p s t w y) -f $(shuf -n 1 -e $(cowsay -l | tail -n +2)) -n

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
alias df='df -h'
alias du='du -c -h -s'
alias mkdir='mkdir -p -v'
alias ping='ping -c 5'
alias sysmon='dstat -cdnpmgs --top-bio --top-cpu'
alias iftop='iftop -B -i wlp3s0'

# sudo commands
if [ $UID -ne 0 ]; then
    alias sudo='sudo '
    alias scat='sudo cat'
    alias snano='sudo nano'
    alias semacs='sudo emacs'
    alias reboot='sudo systemctl reboot'
    alias poweroff='sudo systemctl poweroff'
    alias halt='sudo systemctl halt'
fi

# safety features
alias cp='cp -i'
alias mv='mv -i'
alias ln='ln -i'
alias rm='rm -I'                  # -i prompts for every file
alias chown='chown --preserve-root'
alias chmod='chmod --preserve-root'
alias chgrp='chgrp --preserve-root'

# pacman commands
alias pacadd='pacman -S'
alias pacupgrade='pacman -Syu'    # full system upgrade
alias pacupdate='pacman -Syy'     # sync and update repositories
alias pacsearch='pacman -Ss'      # search
alias pacinfo='pacman -Si'
alias pacremove='pacman -Rs'
alias pacquery='pacman -Qs'
alias paclocal='pacman -Qi'
alias pacaur='pacman -Qqm'
alias pacinstalled='pacman -Qe'
alias pacexplicitdeps='comm -23 <( sort <( pacman -Qe) ) <(sort <( pacman -Qt ) )'
alias pacleaves='pacman -Qt'
alias pacorphans='pacman -Qdt'
alias pacowner='pacman -Qo'
alias pacfiles='pacman -Ql'
alias pacclean='pacman -Sc'
alias pacmake='makepkg -sci'      # make and install AUR package

# cower commands
alias cower='cower -c'
alias cowget='cower -dd'
alias cowupdate='cower -u'
alias cowsearch='cower -s'
alias cowinfo='cower -i'

# connect to an already running ssh agent
ssh-reagent() {
    for agent in /tmp/ssh-*/agent.*; do
        export SSH_AUTH_SOCK="${agent}";
				ssh-add -l 2>&1 > /dev/null
        if [[ $? -eq 0 ]]; then
            echo "Found working SSH Agent" ;
            ssh-add -l ;
            return
        fi
    done
    echo "Cannot find working SSH Agent. Creating new agent";
    eval `ssh-agent`;
}

# use mplayer to stream videos
playstream() {
		/usr/bin/mplayer $1
}

# play streaming videos
youtube() {
		playstream $(/usr/bin/youtube-dl -g $1)
}

#
# set the brightness
brightness() {
		sudo /bin/sh -c "echo $1 > /sys/class/backlight/nvidia_backlight/brightness"
		sudo /bin/sh -c "echo $(((1024 - $1) / 4)) > /sys/class/leds/smc::kbd_backlight/brightness"
}

# pkgfile command not found hook
source /usr/share/doc/pkgfile/command-not-found.zsh

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

PROMPT="%{$fg[white]%}┌─[%{$fg[yellow]%}%*%{$fg[white]%}][%{$fg[magenta]%}%l%{$fg[white]%}][%{$fg[cyan]%}%n%{$fg[white]%}][%{$fg[green]%}%~%{$fg[white]%}]
└───╼ "
RPROMPT="[%?]"
