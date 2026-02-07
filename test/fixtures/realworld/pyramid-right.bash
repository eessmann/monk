#!/usr/bin/env bash
# Source: https://github.com/wolandark/BASH_Scripts_For_Everyone/blob/master/ANSI-Printing/Pyramid-Patterns/1-right-half-pyramid.sh

main()
{
	rows=5;

	for((i=0; i < rows; i++)); do
		for((j=0; j <= i; j++)); do
			echo -n "*"
		done
		echo ""
	done
}
main
