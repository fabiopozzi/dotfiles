#!/bin/bash
rsync -a --delete --info=progress2 -e ssh /home/fabio root@10.0.0.2:/mnt/md0/thinkpad

