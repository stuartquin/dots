# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="clean"
ZSH_THEME="crunch"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)

plugins=(git pass)
source $ZSH/oh-my-zsh.sh
unsetopt correct_all

ZSH_THEME_GIT_PROMPT_PREFIX=""
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[yellow]%}"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[green]%}"

# Customize to your needs...
export PATH=/usr/lib/lightdm/lightdm:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games

alias vim='vim -p'
alias json="python -mjson.tool"
alias jsonifg="python -mjson.tool | less"

export EDITOR=vim
export TERM='xterm-256color'

# test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"

# Pretty grep
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias rgrep='rgrep --color=auto'
alias ack='ack-grep'

alias todo="vim -O ~/Dropbox/epistle/todo.txt ~/Dropbox/epistle/ideas.txt"
alias studo="python /home/stuart/Projects/studo/studo.py"

alias webshare='python -c "import SimpleHTTPServer;SimpleHTTPServer.test()"'
alias ll=' LC_ALL=en_US.UTF-8 LANG=en ls++ '
alias ag='ag --color-line-number=36 --color-path=46 --color-match=42'
alias ccat='pygmentize -g'

# Light coloured vim
alias gviml="gvim -c LuciusLightHighContrast"
alias gvimw="gvim -c LuciusWhiteHighContrast"

# Change dir alias
alias pi='cd ~/Projects/pi/'

# Open a CSV file in csc
function csc() {
  cat $1 | psc -k -d, | sc
}

# VirtualEnv Wrapper stuff
export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$HOME/Devel
source /usr/local/bin/virtualenvwrapper.sh

case $TERM in
    *xterm*)
        precmd () {print -Pn "\e]0;%n@%M: %~\a"}
        ;;
esac

function powerline_precmd() {
   export PS1="$(~/powerline-bash.py $? --shell zsh)"
}

PATH="$PATH:$GOPATH/bin"
export MOZILLA_FIVE_HOME=/usr/lib/mozilla

bindkey -v
bindkey '^P' up-history
bindkey '^N' down-history
bindkey '^?' backward-delete-char
bindkey '^h' backward-delete-char
bindkey '^w' backward-kill-word
bindkey '^r' history-incremental-search-backward

function zle-line-init zle-keymap-select {
    VIM_PROMPT="%{$fg_bold[yellow]%} [% NORMAL]%  %{$reset_color%}"
    RPS1=$'%2~/ $(parse_git_dirty)$(git_prompt_info) %{$reset_color%}${${KEYMAP/vicmd/$VIM_PROMPT}/(main|viins)/}'

    zle reset-prompt
}

zle -N zle-line-init
zle -N zle-keymap-select
export KEYTIMEOUT=1

[ -s $HOME/.nvm/nvm.sh ] && . $HOME/.nvm/nvm.sh # This loads NVM

if type rbenv > /dev/null; then
  # Ruby tat
  alias bi="bundle install --jobs 4 --binstubs .bundle/bin"
  alias be="bundle exec"
  export PATH=".bundle/bin:$PATH"
  export PATH="$HOME/.rbenv/bin:$PATH"
  eval "$(rbenv init -)"
fi

# Set up the prompt
PS1="%{$fg[green]%}%\£ %{$reset_color%}"
RPS1=$'%2~/ $(parse_git_dirty)$(git_prompt_info) %{$reset_color%}'

# Generate gitignores
function gi() { curl -L -s https://www.gitignore.io/api/$@ ;}

# Android SDK
export PATH=${PATH}:~/Apps/android-sdk-linux/tools
export PATH=${PATH}:~/Apps/android-sdk-linux/platform-tools
export PATH=${PATH}:~/.composer/vendor/bin
export PATH=$PATH:/usr/local/go/bin
export PATH=$PATH:$HOME/bin

function gopath() 
{
    mkdir -p bin pkg
    mkdir -p src/github.com/stuart.quin
    export GOPATH=`pwd`
    export PATH=$PATH:$GOPATH/bin
}

export IBUS_ENABLE_SYNC_MODE=1
