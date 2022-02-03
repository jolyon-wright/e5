#!/bin/bash

## sauce.sh

################################################################################
# Copyright Abaco Systems, Inc. (2022)
#
# All rights reserved. No part of this software may be re-produced,
# re-engineered, re-compiled, modified, used to create derivatives, stored in
# a retrieval system, or transmitted in any form or by any means, electronic,
# mechanical, photocopying, recording, or otherwise without the prior written
# permission of Abaco Systems, Inc.
#
#
# Script for .bashrc sourcing
#
################################################################################


if [ -d "$HOME/.guix-profile" ];then
   GUIX_PROFILE="$HOME/.guix-profile"
   . "$GUIX_PROFILE/etc/profile"
   export GUIX_LOCPATH="$HOME/.guix-profile/lib/locale"
fi

if [ $INSIDE_EMACS ]; then
    export PAGER="/bin/cat"
else
    export PAGER="/bin/less"
fi

if [ -d "$HOME/src/boost_1_78_0" ];then
    export BOOST_ROOT="$HOME/src/boost_1_78_0"
fi


# https://www.sevarg.net/2019/04/14/nvidia-jetson-nano-desktop-use-kernel-builds/
# jolyon@xav:~/src/nv/knl/kernel/kernel-4.9$ cat ./scripts/rt-patch.sh


if [ -d "$HOME/.local/bin" ];then
    # appears after a :-
    # pip3 install pyls
    export PATH=$PATH:$HOME/.local/bin
fi

### sauce.sh ends here.
