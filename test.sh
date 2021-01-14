#! /bin/bash

main=bin/main

function main() {
    file=$1

    while IFS=$'\t' read -r input test; do
        output=$(./$main $input)
        if [ $output == $test ]; then
            passed="Passed"
        else
            passed="Failed"
        fi
        echo "${passed}: ${input} -> ${output}"
    done < $file
}

main "$@"