# Check for an interactive session
[ -z "$PS1" ] && return
PATH="/bin:/usr/bin:/sbin:/usr/sbin:/home/edgar/bin/:/opt/bin://opt/openoffice/program/"
#alias ls='ls -a --color=auto'
alias ls='ls --color=always --group-directories-first -lh'
alias lss='ls -a -s'
alias lsd='ls /dev | grep sd'
alias pacman='sudo pacman'
alias snano='sudo nano'
alias wicdd='sudo /etc/rc.d/wicd start'
alias uhg='uzbl -u http://www.google.dk/search?hl=da&q=$1'
#colors=("\[\e[0;31m\]" "\[\e[0;32m\]" "\[\e[0;33m\]" "\[\e[0;34m\]" "\[\e[0;35m\]" "\[\e[0;36m\]" "\[\e[1;30m\]" "\[\e[1;32m\]" "\[\e[1;33m\]" "\[\e[1;34m\]" "\[\e[1;35m\]" "\[\e[1;36m\]")
#PROMPT_COMMAND='let R=$RANDOM%13+0;color=${colors[$R]};PS1="${color}\u@\h:\w\$\[\e[0m\] "'
#PS1='[\u@\h \W]\$ '
export PS1='\[\033[0;32m\]┌┼─┼─ \[\033[0m\033[0;32m\]\u\[\033[0m\] @ \[\033[0;36m\]\h\[\033[0m\033[0;32m\] ─┤├─ \[\033[0m\]\t \d\[\033[0;32m\] ─┤├─ \[\033[0;34m\]\w\[\033[0;32m\] ─┤ \n\[\033[0;32m\]└┼─\[\033[0m\033[0;32m\]\$\[\033[0m\033[0;32m\]─┤▶\[\033[0m\] '
#export PS1='\[\033[0;32m\]┌┼─┼─ \[\033[0m\033[0;32m\]\u\[\033[0m\] @ \[\033[0;36m\]\h\[\033[0m\033[0;32m\] ─┤├─ \[\033[0m\]\t \d\[\033[0;32m\] ─┤├─ \[\033[0;34m\]\w\[\033[0;32m\] ─┤ \n\[\033[0;32m\]└┤[\[\033[0m\033[0;32m\]\$\[\033[0m\033[0;32m\]]▶\[\033[0m\] '
#PS1="\[\033[0;34m\]┌─────────────────────[\[\033[0;36m\]\w\[\033[0;34m\]]\${fill}─────────[\[\033[0;34m\]\T\[\033[0;34m\]]\n└─+\u+ >\[\033[m\]"
export EDITOR="geany"
