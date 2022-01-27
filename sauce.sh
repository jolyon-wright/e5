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
fi

if [ $INSIDE_EMACS ]; then
    export PAGER="/bin/cat"
else
    export PAGER="/bin/less"
fi

### sauce.sh ends here.
