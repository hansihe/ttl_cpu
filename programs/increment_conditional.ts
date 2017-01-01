LOOP:
add 1 acc
mov acc [0]
sub 10 acc
jnez FIN
mov 0 acc
jt LOOP
FIN:
mov [0] acc
jt LOOP
