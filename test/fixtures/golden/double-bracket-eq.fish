if string 'match' '-q' '--' 'foo' (string join ' ' -- $x ; or printf '')
  echo 'ok'
else
  true
end
