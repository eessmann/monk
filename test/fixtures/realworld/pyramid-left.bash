#!/usr/bin/env bash
# Source: https://github.com/wolandark/BASH_Scripts_For_Everyone/blob/master/ANSI-Printing/Pyramid-Patterns/2-left-half-pyramid.sh

main()
{
	rows=5;

	for((i=0; i < rows; i++)); do
		for ((j=0; j < rows - i - 1; j++)) do
			echo -n " "
		done

		for((j=0; j <= i; j++)); do
			echo -n "*"
		done
		echo ""
	done
}
main
