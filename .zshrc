###############################################################################
# general settings (see 'man zshoptions' for the whole list)
#
# NOTE: aliases can be set in any-case, with/without '_', so
# `allexport' is equivalent to `A__lleXP_ort'
#
setopt   aliases                        # expand aliases
setopt   auto_cd                        # dirname<enter> ==> cd dirname
setopt   auto_menu		        # menu after two tabs
setopt   auto_paramslash                # add / to dir names in completion
setopt   autopushd pushdignoredups      # auto add dirs to dirstack, no dups
setopt   complete_aliases               # resolve aliases (before completion)
setopt   correct correctall             # correct my crappy typing + all args
setopt   extendedglob		        # extended globs
setopt   interactivecomments            # allow comments in interactive shells
setopt   nocompletealiases              # otherwise 'agi<TAB>' won't work...

setopt   noprintexitvalue               # don't print exit val when != 0
setopt   transientrprompt	        # right prompt goes away after edit
unsetopt menucomplete		        # don't use menu for completion
################################################################################

################################################################################
# history
#
export HISTSIZE=5000		        # max size of history
export SAVEHIST=1000
export HISTFILE=~/.zsh/zhistory         # place to store the history
setopt appendhistory                    # append history with session
setopt incappendhistory                 # ...continually (not just upon exit)
setopt sharehistory                     # share history between sessions
setopt histignorealldups                # no dups in the history
setopt histnostore                      # don't put 'history' in history
setopt extendedhistory                  # put timestamps in history
setopt equals                           # allow =cmd instead of which cmd
setopt histignorespace                  # ignore commands with leading space
################################################################################

zstyle :compinstall filename '/home/chirantan/.zshrc'

autoload -U compinit
compinit

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
alias psgrep='ps aux | grep'
alias df='df -h'
alias du='du -c -h -s'
alias mkdir='mkdir -p -v'
alias ping='ping -c 5'
alias dstat='dstat -cdnpmgs --top-bio --top-cpu'
alias iftop='sudo iftop -B -i wlp3s0'

alias reboot='systemctl reboot'
alias poweroff='systemctl poweroff'
alias halt='systemctl halt'

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

# systemctl --user is too long to type every time
alias user='systemctl --user'

# mplab path
export PATH=/opt/microchip/xc16/v1.20/bin:$PATH

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

# use mplayer to stream videos
playstream() {
    /usr/bin/mplayer $(/usr/bin/youtube-dl -g $1)
}

# set the brightness
brightness() {
    sudo /bin/sh -c "echo $1 > /sys/class/backlight/nvidia_backlight/brightness"
    sudo /bin/sh -c "echo $(((1024 - $1) / 4)) > /sys/class/leds/smc::kbd_backlight/brightness"
}

# list all the targets of the makefile in the current directory
list-make-targets() {
    make -qp | awk -F':' '/^[a-zA-Z0-9][^$#\/\t=]*:([^=]|$)/ {split($1,A,/ /);for(i in A)print A[i]}'
}

send-cmd() {
    sudo sh -c "echo $1 > /dev/rfcomm$2"
}

# pkgfile command not found hook
source /usr/share/doc/pkgfile/command-not-found.zsh

################################################################################
# prompt
#
# this gets us the symbolic color names and terminal details
autoload colors zsh/terminfo
colors

setopt prompt_subst # do parameter expanse etc. on *PROMPT

# last 2 pwd components,with HOME replaced by ~
# NOTE: color refs should be escaped in %{ %}, or zsh
# will miscalculate the length (and prompt gets misaligned)
local prpath="%{${fg[green]}%}%2~%{${fg[default]}%}"
# errcode of lst cmd if nonzero
local prerr="%(0?..[%{${fg[red]}%}%?%{${fg[default]}%}])"
local pruser="%{${fg[blue]}%}%n%{${fg[default]}%}"         # username
local prhost="%{${fg[magenta]}%}%m%{${fg[default]}%}"        # hostname


# get the vcs (git etc.) info we use in the prompt
autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git bzr svn hg
local vcstool="%{${fg[white]}%}%s%{${fg[default]}%}" # "git", "bzr", ....
local vcsproj="%{${fg[green]}%}%r%{${fg[default]}%}" # top level dirname
local vcsbranch="%{${fg[yellow]}%}%b%{${fg[default]}%}"
local vcsaction="%{${fg[red]}%}%a%{${fg[default]}%}"

zstyle ':vcs_info:git*' formats "(${vcstool})-[${vcsbranch}] ${vcsproj}"
zstyle ':vcs_info:git*' actionformats "(${vcstool})-[${vcsbranch}|${vcsaction}] ${vcsproj}"

# this function will be called before the prompt is calculated;
# needed to get the updated git/bzr info
function precmd() {vcs_info}

# this leftside prompt
# which will look something like 'djcb@borealis:~ % '
PROMPT='┌─[${pruser}][${prhost}][${prpath}]
└─${prerr}──╼ '
RPROMPT='${vcs_info_msg_0_}'

################################################################################

source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
