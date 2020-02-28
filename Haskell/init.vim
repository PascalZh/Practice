set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath = &runtimepath

set buftype=nofile
set noswapfile
set nowrap
set nomodifiable
map q :!bspc desktop -f last<enter>:quit<enter>
map i :e ~/.config/sxhkd/sxhkdrc<enter>:unmap i<enter>:set modifiable<enter>:set buftype=<enter>
imap jk <ESC>

syntax match myFrame /[┌─┐│└┴┘┼┬├┤]*/
hi link myFrame Comment
au BufEnter * exe "normal! /^│\zs\(\s\\|\k\)*\ze│$<enter>"
