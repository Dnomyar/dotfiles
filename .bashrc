export EDITOR="vim"
export PATH="$PATH:/home/damien/.gem/ruby/2.2.0/bin"
export PATH="$PATH:$(ruby -rubygems -e "puts Gem.user_dir")/bin"
exec zsh
