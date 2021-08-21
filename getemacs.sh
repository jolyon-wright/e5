#!/bin/bash

## getemacs.sh

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

#!/bin/bash

eval `ssh-agent -s`
ssh-add ~/.ssh/*_rsa

git config --global user.email "jolyon.wright@abaco.com"
git config --global user.name "Jolyon Wright"


sudo add-apt-repository ppa:kelleyk/emacs
sudo apt update
sudo apt install -y emacs27
#sudo apt install -y git build-essential

# pull lisp
#git clone git@bitbucket.org:jolyon929/e5.git --single-branch .emacs.d
# if [ $? -ne 0 ]; then
#     print_err "clone failed"
#     exit 1
# fi

cat << EOF >> ~/.bashrc
if [ \$INSIDE_EMACS ]; then
    export PAGER="/bin/cat"
else
    export PAGER="/bin/less"
fi

export GIT_SERVER="git@bitbucket.org:"
export GS=git@bitbucket.org:jolyon929
export TS=git@towgit01:gvc1001
export EDITOR='emacs -Q'
EOF


exit 0

### getemacs.sh ends here.
