#!/bin/bash

## eminit.sh

################################################################################
# Copyright Abaco Systems, Inc. (2021)
#
# All rights reserved. No part of this software may be re-produced,
# re-engineered, re-compiled, modified, used to create derivatives, stored in
# a retrieval system, or transmitted in any form or by any means, electronic,
# mechanical, photocopying, recording, or otherwise without the prior written
# permission of Abaco Systems, Inc.
#
#
# Script to
#
################################################################################

PROGRAM_NAME="$(basename "$0")"
PROGRAM_DIR="$(cd "$(dirname "$0")"; pwd;)"

################################################################################
# Main

TARGET="gvc"
USER="gvc"
IP="192.168.1.100"

function print_err {
    echo -e "\n ERROR: $@" >&2
}

while getopts "t:u:" opt; do
    case $opt in
        t)
            TARGET="${OPTARG}"
            ;;
        u)
            USER="${OPTARG}"
            ;;
        i)
            IP="${OPTARG}"
            ;;
        :)
            print_err "Option -${OPTARG} requires an argument."
            exit 1
            ;;
    esac
done
# [ -n "${USER}" ] || { print_err "Please specify option -u"; exit 1; }
# [ -n "${TARGET}" ] || { print_err "Please specify option -t"; exit 1; }

ssh-keygen -R "${IP}"
ssh-keygen -R "${TARGET}"
cat ~/.ssh/id_rsa.pub | ssh "${USER}@${TARGET}" "mkdir -p ~/.ssh && cat >> ~/.ssh/authorized_keys && chmod 600 ~/.ssh/authorized_keys"
scp -r ~/.ssh/ "${USER}@${TARGET}:/home/${USER}"
scp *.sh ${USER}@${TARGET}:/home/${USER}
scp ~/k.sh ${USER}@${TARGET}:/home/${USER}
scp .emacs ${USER}@${TARGET}:/home/${USER}
#scp ~/.emacs.d/* ${USER}@${TARGET}:/home/${USER}/.emacs.d
ssh ${USER}@${TARGET}



exit 0

### eminit.sh ends here.
