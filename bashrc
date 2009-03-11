# Check for an interactive session
[ -z "$PS1" ] && return
PATH="/bin:/usr/bin:/sbin:/usr/sbin:/home/edgar/bin/:/opt/bin"
alias ls='ls -a --color=auto'
alias lss='ls -a -s'
alias pacman='sudo pacman'
alias snano='sudo nano'
colors=("\[\e[0;30m\]" "\[\e[0;31m\]" "\[\e[0;32m\]" "\[\e[0;33m\]" "\[\e[0;34m\]" 
"\[\e[0;35m\]" "\[\e[0;36m\]" "\[\e[0;37m\]" "\[\e[1;30m\]" "\[\e[1;31m\]" 
"\[\e[1;32m\]" "\[\e[1;33m\]" "\[\e[1;34m\]" "\[\e[1;35m\]" "\[\e[1;36m\]" "\[\e[1;37m\]")

PROMPT_COMMAND='let R=$RANDOM%16+0;color=${colors[$R]};PS1="${color}\u@\h:\w\$\[\e[0m\] "'
PS1='[\u@\h \W]\$ '
