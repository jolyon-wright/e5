#! /usr/bin/bash
cd ~
mkdir p4
cd p4
wget https://package.perforce.com/perforce.pubkey
gpg --with-fingerprint perforce.pubkey
wget -qO - https://package.perforce.com/perforce.pubkey | sudo apt-key add -
sudo cp ./perforce.list  /etc/apt/sources.list.d/perforce.list
sudo apt-get update
sudo apt-get install helix-p4d

sudo /opt/perforce/sbin/configure-helix-p4d.sh
