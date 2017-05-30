# Linux specific aliases
if [[ "$os" = "$linux_str" ]]; then
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'
    # Pacman aliases
    alias pacman=run_pacman
    # ssh aliases
    alias usshfs='fusermount -u $HOME/Remote'
elif [[ "$os" = "$osx_str" ]]; then
    alias ls='ls -G'
    # ssh aliases
    alias usshfs='umount $HOME/Remote'
fi

alias nv=nvim

alias tm=trash

# man aliases
alias man=run_man

# Compression aliases
alias tarx='tar -xvf'
alias targz='tar -zxvf'
alias tarbz2= 'tar -jxvf'

# Aliases for stack managed executables
alias sghc='stack ghc'
alias srunghc='stack runghc'
