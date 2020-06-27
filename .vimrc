set makeprg=cabal
nnoremap <Leader>m :make v2-build -f development<CR>
iabbrev >< ×
iabbrev o ∘
function MakePath(...)
  return join(systemlist('find ' . join(a:000, ' ') . ' -type d'),',')
endfunction
let &path=MakePath('src')
