#!/bin/bash

#https://www.perforce.com/downloads/helix-visual-client-p4v
cat << EOF >> ~/.bashrc
export PATH=$PATH:~/p4v-2021.1.2085655/bin/p4v

EOF


cd ~
mkdir p4
pushd p4
wget https://package.perforce.com/perforce.pubkey
gpg --with-fingerprint perforce.pubkey
wget -qO - https://package.perforce.com/perforce.pubkey | sudo apt-key add -
popd
sudo cp ./perforce.list  /etc/apt/sources.list.d/perforce.list
sudo apt-get update
sudo apt-get install helix-p4d

#sudo /opt/perforce/sbin/configure-helix-p4d.sh
