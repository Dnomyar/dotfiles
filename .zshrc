# Path to your oh-my-zsh installation.
export ZSH=/Users/damien/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="powerlevel9k/powerlevel9k"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git colored-man brew osx zsh-syntax-highlighting boot2docker cabal sbt colorize bower chucknorris mvn scala sudo docker z) 

# User configuration

export PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin:/Library/TeX/texbin:/home/damien/activator:/usr/local/sbin:/Users/damien/bin/stack-1.0.4-osx-x86_64:/Users/damien/.local/bin:/Users/damien/.cabal/bin"
# export MANPATH="/usr/local/man:$MANPATH"

source $ZSH/oh-my-zsh.sh

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"





#source ~/.zsh/zsh-git-prompt/zshrc.sh

# https://github.com/rpellerin/dotfiles/blob/master/.aliases
# Extract any archive
function extract() {
    if [ -f $1 ] ; then
        case $1 in
            *.tar.bz2) tar xjf $1 ;;
            *.tar.gz) tar xzf $1 ;;
            *.bz2) bunzip2 $1 ;;
            *.rar) rar x $1 ;;
            *.gz) gunzip $1 ;;
            *.tar) tar xf $1 ;;
            *.tbz2) tar xjf $1 ;;
            *.tgz) tar xzf $1 ;;
            *.zip) unzip $1 ;;
            *.Z) uncompress $1 ;;
            *) echo "'$1' cannot be extracted via extract()" ;;
        esac
    else
        echo "'$1' is not a valid file"
    fi
}

# A reminder
function githelp {
    echo "-------------------------------------------------------------------------------"
    echo "git clone http://... [repo-name]"
    echo "git init [repo-name]"
    echo "-------------------------------------------------------------------------------"
    echo "git add -A <==> git add . ; git add -u # Add to the staging area (index)"
    echo "-------------------------------------------------------------------------------"
    echo "git commit -m 'message' -a"
    echo "git commit -m 'message' -a --amend"
    echo "-------------------------------------------------------------------------------"
    echo "git status"
    echo "git log --stat # Last commits, --stat optional"
    echo "git ls-files"
    echo "git diff HEAD~1..HEAD"
    echo "-------------------------------------------------------------------------------"
    echo "git push origin master"
    echo "git push origin master:master"
    echo "-------------------------------------------------------------------------------"
    echo "git remote add origin http://..."
    echo "git remote set-url origin git://..."
    echo "-------------------------------------------------------------------------------"
    echo "git stash"
    echo "git pull origin master"
    echo "git stash list ; git stash pop"
    echo "-------------------------------------------------------------------------------"
    echo "git submodule add /absolute/path repo-name"
    echo "git submodule add http://... repo-name"
    echo "-------------------------------------------------------------------------------"
    echo "git checkout -b new-branch <==> git branch new-branch ; git checkout new-branch"
    echo "git merge old-branch"
    echo "git branch local_name origin/remote_name # Associate branches"
    echo "-------------------------------------------------------------------------------"
    echo "git update-index --assume-unchanged <file> # Ignore changes"
    echo "git rm --cached <file> # Untrack a file"
    echo "-------------------------------------------------------------------------------"
    echo "git reset --hard HEAD # Repair what has been done since last commit"
    echo "git revert HEAD # Repair last commit"
    echo "git checkout [file] # Reset a file to its previous state at last commit"
    echo "-------------------------------------------------------------------------------"
    echo "git tag # List"
    echo "git tag v0.5 # Lightwieght tag"
    echo "git tag -a v1.4 -m 'my version 1.4' # Annotated tag"
    echo "git push origin v1.4 # Pushing"
    echo "-------------------------------------------------------------------------------"
    echo "HOW TO RENAME A BRANCH LOCALLY AND REMOTELY"
    echo "git branch -m old_name new_name"
    echo "git push origin new_name"
    echo "git push origin :old_name"
    echo "------"
    echo "Each other client of the repository has to do:"
    echo "git fetch origin ; git remote prune origin"
    echo "-------------------------------------------------------------------------------"
}

function custom_cd () {
    cd "$@" && ls -GlaAh
}


# Alias
alias ls='ls -AhG'
alias l="ls -GlaAh"
alias grep='grep -i --color=auto'
alias rm='rm -vi'
alias mv='mv -vi'
alias cp='cp -Rv'
alias mkdir='mkdir -p'
alias ...='../..'
alias ..='cd ..'
alias path='echo $PATH | tr -s ":" "\n"'
alias cd='custom_cd'
alias lstree="find . | sed 's/[^/]*\//| /g;s/| *\([^| ]\)/+--- \1/'"

# alias code="visual-studio-code"
alias sha256="shasum -a 256"
alias wiresharkk="sudo chmod 644 /dev/bpf* && wireshark"
alias redis.start="redis-server /usr/local/etc/redis.conf"
alias pg.start="postgres -D /usr/local/var/postgres"
alias composer='docker run -ti --rm -v $(pwd):/app composer/composer'

function readFromYT () {
    /Applications/VLC.app/Contents/MacOS/VLC "$(yturl $@ -q high)"
}
alias yt='readFromYT'


# Git
function gcob () {
    if [ -z $1 ] ; then
	echo "Bad usage: 'gcob <branch name>'"
    else
        git checkout -b $1
    fi
}

function gfeat () {
    if [ -z $1 ] ; then
        echo "Bad usage: 'gfeat <feature branch name>' (this will create a branch name 'feature/<branch name>)"
    else
        git checkout -b feature/$1
    fi
}

function gcm () {
    if [ -z $1 ] ; then
        echo "Bad usage: 'gcm <commit message>'"
    else
        git commit -m $1
    fi
}


#################################################

UTILISATEUR=%n
# THIS_DIRECTORY=%~
THIS_DIRECTORY=%c

#GIT_PROMPT_EXECUTABLE="haskell"

# PROMPT="${UTILISATEUR} | ${THIS_DIRECTORY} $ "
# PROMPT='%B%m | %c%b$(git_super_status) $ '
#PROMPT='%{$fg[magenta]%}%B%m%b%{$reset_color%} | %c %B%{$fg[green]%}$%{$reset_color%}%b '
#RPROMPT='$(git_super_status)'

# enable colored output from ls, etc
export CLICOLOR=1

export PATH=$PATH:/home/damien/activator


autoload -U compinit promptinit
compinit
promptinit



setopt append_history # append rather then overwrite
setopt extended_history # save timestamp
setopt inc_append_history # add history immediately after typing a command

ssh-add ~/.ssh/id_perso
ssh-add ~/.ssh/id_noeupapp

chuck | cowsay 

export PATH="/usr/local/opt/postgresql@9.5/bin:$PATH"
export PATH="/usr/local/opt/node@6/bin:$PATH"

export TERM="xterm-256color"
