#!/bin/bash

docker run --interactive --tty --rm \
       --env DISPLAY=$DISPLAY \
       --volume /tmp/.X11-unix:/tmp/.X11-unix \
       --volume $(pwd)/package:/home/developer/.config/emacs/user-config \
       --volume $(pwd)/elpa:/home/developer/.config/emacs/elpa \
       --volume $(pwd)/templates/early-init.el.template:/home/developer/.config/emacs/early-init.el \
       --volume $(pwd)/templates/init.el.template:/home/developer/.config/emacs/init.el \
       --volume $HOME/.ssh:/home/developer/.ssh \
       emacs-sandbox:latest emacs $@
