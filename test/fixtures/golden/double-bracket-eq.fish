if string 'match' '-q' '--' 'foo' (string join ' ' $x)
  echo 'ok'
else
  begin
    true
  end
end
