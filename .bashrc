#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Arch latest news
# The characters "£, §" are used as metacharacters. They should not be encountered in a feed...
echo -e "$(echo $(curl --silent https://www.archlinux.org/feeds/news/ | xmlindent | sed -e '6,${/<item>/,/<\/item>/d;}' | \
                  sed -e ':a;N;$!ba;s/\n/ /g') | \
           sed -e 's/&amp;/\&/g
           s/&lt;\|&#60;/</g
           s/&gt;\|&#62;/>/g
           s/<\/a>/£/g
           s/href\=\"/§/g
           s/<title>/\\n\\n\\n   :: \\e[01;31m/g; s/<\/title>/\\e[00m ::\\n/g
           s/<link>/ [ \\e[01;36m/g; s/<\/link>/\\e[00m ]/g
           s/<description>/\\n\\n\\e[00;37m/g; s/<\/description>/\\e[00m\\n\\n/g
           s/<p\( [^>]*\)\?>\|<br\s*\/\?>/\n/g
           s/<b\( [^>]*\)\?>\|<strong\( [^>]*\)\?>/\\e[01;30m/g; s/<\/b>\|<\/strong>/\\e[00;37m/g
           s/<i\( [^>]*\)\?>\|<em\( [^>]*\)\?>/\\e[41;37m/g; s/<\/i>\|<\/em>/\\e[00;37m/g
           s/<u\( [^>]*\)\?>/\\e[4;37m/g; s/<\/u>/\\e[00;37m/g
           s/<code\( [^>]*\)\?>/\\e[00m/g; s/<\/code>/\\e[00;37m/g
           s/<a[^§|t]*§\([^\"]*\)\"[^>]*>\([^£]*\)[^£]*£/\\e[01;31m\2\\e[00;37m \\e[01;34m[\\e[00;37m \\e[04m\1\\e[00;37m\\e[01;34m ]\\e[00;37m/g
           s/<li\( [^>]*\)\?>/\n \\e[01;34m*\\e[00;37m /g
           s/<!\[CDATA\[\|\]\]>//g
           s/\|>\s*<//g
           s/ *<[^>]\+> */ /g
           s/[<>£§]//g')\n\n";


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
alias df='df -h'
alias du='du -c -h -s'
alias mkdir='mkdir -p -v'
alias ping='ping -c 5'
alias xp='xprop | grep "WM_WINDOW_ROLE\|WM_CLASS" && echo "WM_CLASS(STRING) = \"NAME\", \"CLASS\""'

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
alias pacman='pacman-color'
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

# command-not-found hook
command_not_found_handle () {
    local pkgs cmd=$1

    mapfile -t pkgs < <(pkgfile -bv -- "$cmd" 2>/dev/null)
    if (( ${#pkgs[*]} )); then
        printf '%s may be found in the following packages:\n' "$cmd"
        printf '  %s\n' "${pkgs[@]}"
        return 0
    else
        printf "bash: $(gettext bash "%s: command not found")\n" "$cmd" >&2
        return 127
    fi
}

# cd and ls together command
cl() {
    if [ -d "$1" ]; then
        cd $1
        ls
        return 0
    else
        printf "bash: cl: $1: Directory not found" >&2
        return 127
    fi
}

# connect to an already running ssh agent
ssh-reagent() {
    for agent in /tmp/ssh-*/agent.*; do
        export SSH_AUTH_SOCK="${agent}";
        if [[ `ssh-add -l 2> /dev/null` ]]; then
            echo "Found working SSH Agent" ;
            ssh-add -l ;
            return
        fi
    done
    echo "Cannot find working SSH Agent. Creating new agent";
    eval `ssh-agent`;
}

# make tags in specified directory
mktags() {
    if [ -d "$1" ]; then
        cd "$1" && etags `find . -name "*.[h|hpp|cpp|c]"` && cd -
        return 0
    else
        printf "bash: mktags: $1: Directory not found" >&2
        return 127
    fi
}

# PS1 code
blue="\[\e[0;36m\]"
red="\[\e[0;31m\]"
white="\[\e[0;37m\]"
orange="\[\e[0;33m\]"
green="\[\e[0;32m\]"
pink="\[\e[0;35m\]"
tty=`tty 2>/dev/null`

if [ `id -u` -eq "0" ]; then
    root="${red}"
else
    root="${blue}"
fi

PS1="${white}┌─[${orange}\t${white}][${pink}${tty}${white}][${root}\u${white}][${green}\w${white}]\n${white}└───╼ \[\e[0m\]"
