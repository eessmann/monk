#!/usr/bin/env bash
printf '<%s>\n' {alpha,beta}
printf '<%s>\n' pre{1,2}post
left=alpha
right=beta
printf '<%s>\n' {$left,$right}/{one,two}
prefix=pre
printf '<%s>\n' ${prefix}{1,2}
