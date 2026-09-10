#!/usr/bin/env python3
"""Compare the shared digest against the independently installed sha256sum."""
import random, subprocess, sys
rng=random.Random(20260910)
for size in [0,1,3,55,56,57,63,64,65,127,128,129,1024,65536,1000000]:
 data=rng.randbytes(size)
 actual=subprocess.check_output([sys.argv[1],'--digest'],input=data)
 expected=subprocess.check_output(['sha256sum'],input=data).split()[0]
 assert actual==expected,(size,actual,expected)
print('SHA256 sha256sum differential sizes 0..1000000 passed')
