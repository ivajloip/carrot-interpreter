map <F6> :w<CR>:%Eval<CR>
map <F5> :w<CR>:%Eval<CR>:execute "Eval" 
      \ "(run-tests 'carrot-interpreter." . TestName() . ")"<CR>

fun TestName() "{{{
  let name = expand('%:t')
  let name = substitute(name, ".clj$", "", "")
  let name = substitute(name, "_", "-", "")

  if match(name, ".*-test") != 0
    let name = name . "-test"
  endif

  return name 
endfunction "}}}
