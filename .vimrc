set makeprg=cabal
nnoremap <Leader>m :make v2-build -f development<CR>
iabbrev >< ×
iabbrev o ∘
let &path='.,' . join(systemlist('find src -type d'),',')

