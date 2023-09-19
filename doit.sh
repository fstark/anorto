#!/bin/bash

# z80dasm -l -t -g 0 -a Otrona_Attaché_U252_Rev_D.BIN > otrona_d2.asm

while true; do
    inotifywait -qq -e close_write otrona_d.asm
    echo -n "Checking..."
    # z80asm otrona_d.asm -o otrona_d.bin
    pasmo --alocal otrona_d.asm otrona_d.bin
    echo "done"

    if [ -f otrona_d.bin ]; then
        if ! diff -q otrona_d.bin Otrona_Attaché_U252_Rev_D.BIN; then
            echo "ERROR!"
            hexdump Otrona_Attaché_U252_Rev_D.BIN > /tmp/a.hex
            hexdump otrona_d.bin > /tmp/b.hex
            # git diff -U0  --no-index --no-prefix --word-diff=plain --word-diff --word-diff-regex=. /tmp/a.hex /tmp/b.hex
            diff /tmp/a.xxd /tmp/b.xxd | head -10
        else
            cp otrona_d.asm otrona_d_lastgood.asm
        fi
    fi
done
