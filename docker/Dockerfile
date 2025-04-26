FROM ubuntu:22.04
LABEL maintainer="developer@frenkel.org.il"

# Arguments
ARG APP_USER=developer
ARG APP_APT_PACKAGES=apt-requirements.txt
ARG APP_OHMYZSH_URL=https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh
ARG APP_UID=1234

ENV APP_HOME=/home/${APP_USER}
ENV APP_TEMP=/tmp
ENV APP_SHELL=/usr/bin/zsh

ADD *-requirements.txt ${APP_TEMP}/

# APT
RUN export DEBIAN_FRONTEND=noninteractive
RUN apt-get update -yy
RUN apt-get upgrade -yy 
RUN apt-get install -y $(sed 's/#.*//' ${APP_TEMP}/${APP_APT_PACKAGES} | xargs)
RUN useradd -mUu ${APP_UID} -G sudo --shell ${APP_SHELL} ${APP_USER}
RUN echo "en_US.UTF-8 UTF-8" > /etc/locale.gen
RUN locale-gen
RUN mkdir -p ${APP_HOME}/.ssh
RUN echo "%sudo ALL=(ALL:ALL) NOPASSWD: ALL" > /etc/sudoers.d/wheel

# Clean
RUN apt-get clean
# RUN <<EOT
#     rm -rf *-requirements.txt \
#     ${APP_TEMP}/* \
#     /var/lib/apt/lists/*
# EOT

# COPY zshrc.zsh-template ${APP_HOME}/.zshrc

# Entrypoint
USER ${APP_USER}
ADD --chown=${APP_USER}:${APP_USER} ${APP_OHMYZSH_URL} ${APP_TEMP}
RUN sh /tmp/install.sh --unattended
ADD --chown=${APP_USER}:${APP_USER} zshrc.zsh-template ${APP_HOME}/.zshrc
RUN echo "i3" > ~/.xsession

RUN mkdir -p ~/.config/terminology/config/standard/
ADD --chown=${APP_USER}:${APP_USER} terminology-base.cfg.template ${APP_HOME}/.config/terminology/config/standard/base.cfg
# RUN mkdir -p ~/.config/terminology/config/standard/
# RUN eet -e ~/.config/terminology/config/standard/base.cfg config ${APP_TEMP}/terminology-base.cfg.template compress
# RUN cat > ~/.config/terminology/config/standard/base.cfg <<EOF
#     group "Config" struct {
#     value "font.name" string: "Fira Code:style=Regular";
#     value "font.size" int: 14;
#     }
# EOF


WORKDIR ${APP_HOME}
CMD [ ${APP_SHELL} ]
