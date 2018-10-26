#!/bin/bash

RISCV_TOOLS=$HOME/BTSync/riscv-tools
RV32_ISA=$RISCV_TOOLS/riscv-tests/isa
for x in $RV32_ISA/rv64ui-{p,v}-*.hex
do
    echo $x 1>&2
    echo $(./multisim $(echo $x|sed 's,.hex$,,') 2>&1 |grep 'HOST RESULT') $x
done | tee raw.log | sort | grep -v 'HOST RESULT 1 ' | tee regressions64.log
