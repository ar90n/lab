FROM mcr.microsoft.com/devcontainers/go:0-1-bullseye

ARG UID
ARG GID

RUN usermod -u ${UID} vscode
RUN groupmod -g ${GID} vscode