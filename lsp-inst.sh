#!/bin/bash

## lsp-inst.sh

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
# Script to setup lsp stuff
#
################################################################################

PROGRAM_NAME="$(basename "$0")"
PROGRAM_DIR="$(cd "$(dirname "$0")"; pwd;)"

################################################################################
# Main


sudo apt install npm
sudo npm cache clean -f
sudo npm install -g n
sudo n stable


exit 0

### lsp-inst.sh ends here.
