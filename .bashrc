
# Check for an interactive session
[ -z "$PS1" ] && return
export PATH="/usr/share/perl5/vendor_perl/auto/share/dist/Cope:/bin:/usr/bin:/sbin:/usr/sbin:$HOME/bin:/usr/bin/perlbin/core/:/opt/java/jre/bin/:/opt/android-sdk/tools/:/usr/bin/perlbin/vendor/"
alias tree="ls -R | grep ":$" | sed -e 's/:$//' -e 's/[^-][^\/]*\//--/g' -e 's/^/   /' -e 's/-/|/'"
alias ls='ls --color=auto'
alias pacman='sudo pacman'
alias svi='sudo vim'
alias vi='vim'
alias snano='sudo nano'
alias rscp='rsync -aP --no-whole-file --inplace'
#ssh aliases
alias pacman32="sudo pacman --root /opt/arch32 --cachedir /opt/arch32/var/cache/pacman/pkg --config /opt/arch32/pacman.conf"
#PS1='[\u@\h \W]\$ '
#export PS1='\[\033[0;32m\]┌┼─┼─ \[\033[0m\033[0;35m\]\u\[\033[0m\] @ \[\033[0;36m\]\h\[\033[0m\033[0;32m\] ─┤├─ \[\033[0m\]\t \d\[\033[0;32m\] ─┤├─ \[\033[0;34m\]\w\[\033[0;32m\] ─┤ \n\[\033[0;32m\]└┼─\[\033[0m\033[0;32m\]\$\[\033[0m\033[0;32m\]─┤▶\[\033[0m\] '
PS1='[\[\033[0;34m\]\u\[\033[0;33m\]@\[\033[0;34m\]\h\[\033[1;31m\] :\w\[\033[1;37m\]\[\033[0m\]] > \[\033[0m\]'
#PS2='\\ '
# colors function
function colors()
{
  for NUMBER in $(seq 0 15); do
    printf "\e[0;38;5;${NUMBER}m%4d" $NUMBER
  done
  echo ""
  
  if [ -z "$1" ]; then
  
    for _I in $(seq 0 5); do
      for _J in $(seq 0 5); do
        for _K in $(seq 0 5); do
          NUMBER=$((16 + $_I + 6 * $_J + 36 * $_K ))
          printf "\e[0;38;5;${NUMBER}m%4d" $NUMBER
        done
        echo -n " "
        for _K in $(seq 0 5); do
          NUMBER=$((16 + 36 * $_I + $_J + 6 * $_K))
          printf "\e[0;38;5;${NUMBER}m%4d" $NUMBER
        done
        echo -n " "
        for _K in $(seq 0 5); do
          NUMBER=$((16 + 6 * $_I + 36 * $_J + $_K))
          printf "\e[0;38;5;${NUMBER}m%4d" $NUMBER
        done
        echo ""
      done
      echo ""
    done
  
  else
  
    for _I in $(seq 0 5); do
    for _J in $(seq 0 5); do
    for _K in $(seq 0 5); do
      NUMBER=$((16 + 6 * $_I + $_J + 36 * $_K))
      printf "\e[0;38;5;${NUMBER}m%4d" $NUMBER
      #echo -n " $_I$_J$_K"
    done
    echo -n " "
    done
    echo ""
    done
    echo ""
  
    for _I in $(seq 0 5); do
    for _J in $(seq 0 5); do
    for _K in $(seq 0 5); do
      NUMBER=$((16 + $_I + 36 * $_J + 6 * $_K))
      printf "\e[0;38;5;${NUMBER}m%4d" $NUMBER
      #echo -n " $_I$_J$_K"
    done
    echo -n " "
    done
    echo ""
    done
    echo ""
  
    for _I in $(seq 0 5); do
    for _J in $(seq 0 5); do
    for _K in $(seq 0 5); do
      NUMBER=$((16 + 36 * $_I + 6 * $_J + $_K))
      printf "\e[0;38;5;${NUMBER}m%4d" $NUMBER
      #echo -n " $_I$_J$_K"
    done
    echo -n " "
    done
    echo ""
    done
    echo ""
  
  fi
  
  for NUMBER in $(seq 232 255); do
    printf "\e[0;38;5;${NUMBER}m%4d" $NUMBER
  done
echo -e "\n"
}

# X Terminal titles
case "$TERM" in
xterm*|rxvt*)
        PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME}: ${PWD}\007"'
        ;;
*)
        ;;
esac

# Colorize man pages
export LESS_TERMCAP_mb=$'\E[01;31m'          # begin blinking
export LESS_TERMCAP_md=$'\E[01;38;5;74m'  # begin bold
export LESS_TERMCAP_me=$'\E[0m'                # end mode
export LESS_TERMCAP_se=$'\E[0m'                # end standout-mode
export LESS_TERMCAP_so=$'\E[38;5;246m'      # begin standout-mode - info box
export LESS_TERMCAP_ue=$'\E[0m'                # end underline
export LESS_TERMCAP_us=$'\E[04;38;5;146m' # begin underline

