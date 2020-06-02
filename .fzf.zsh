# Setup fzf
# ---------
if [[ ! "$PATH" == */home/stuart/.fzf/bin* ]]; then
  export PATH="${PATH:+${PATH}:}/home/stuart/.fzf/bin"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/home/stuart/.fzf/shell/completion.zsh" 2> /dev/null

# Key bindings
# ------------
source "/home/stuart/.fzf/shell/key-bindings.zsh"
