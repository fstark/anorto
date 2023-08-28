#!/bin/bash

# z80dasm -l -t -g 0 -a Otrona_Attaché_U252_Rev_D.BIN > otrona_d2.asm

while true; do
    inotifywait -qq -e close_write otrona_d.asm
    echo -n "Checking..."
    z80asm otrona_d.asm -o otrona_d.bin
    echo "done"

    if ! diff -q otrona_d.bin Otrona_Attaché_U252_Rev_D.BIN; then
        echo "ERROR!"
        xxd Otrona_Attaché_U252_Rev_D.BIN > /tmp/a.xxd
        xxd otrona_d.bin > /tmp/b.xxd
        diff /tmp/a.xxd /tmp/b.xxd | head -10
    else
        cp otrona_d.asm otrona_d_lastgood.asm
    fi
done
