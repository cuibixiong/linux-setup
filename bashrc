if [ -t 1 ]; then
source ~/.bash_completion.d/contrib_completion_git-completion.bash

## Setting colors
c_red=`tput setaf 1`
c_green=`tput setaf 2`
c_yellow=`tput setaf 3`
c_blue=`tput setaf 4`
c_purple=`tput setaf 5`
c_cyan=`tput setaf 6`
c_white=`tput setaf 7`
c_sgr0=`tput sgr0`
c_bold=`tput bold`
c_ul=`tput smul`

## In the middle
PS1='[\u@\h \[${c_cyan}\]\w\[${c_sgr0}\]$(__git_ps1 " (\[${c_green}\]%s\[${c_sgr0}\])")]\$ '

bind '"\x1b\x5b\x41":history-search-backward'
bind '"\x1b\x5b\x42":history-search-forward'
fi


if [ "$PS1" ]; then
case $TERM in
xterm*)
PROMPT_COMMAND='history -a; echo -ne "\033]0;${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/~}"; echo -ne "\007"'
;;
screen*)
PROMPT_COMMAND='history -a; echo -ne "\033k\033\\"'
;;
*)
PROMPT_COMMAND='history -a'
;;
esac
fi
