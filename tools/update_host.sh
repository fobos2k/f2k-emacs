#/usr/bin/env bash

set -euo pipefail

# Find the directory of this script from anywhere in the project tree
script_name="$(basename "$0")"
script_path="$(find ./ -name "$script_name" -type f -print -quit)"

if [[ -z "$script_path" ]]; then
    echo "Error: script '$script_name' not found in current directory tree." >&2
    exit 1
fi

SOURCE_DIR="$(cd "$(dirname "$script_path")/.." && pwd)"
EMACS_CONFIG_PATH="${HOME}/.config/emacs"
USER_CONFIG_PATH="${EMACS_CONFIG_PATH}/user-config"
SOURCE_TEMPLATES="${SOURCE_DIR}/templates"
SOURCE_PACKAGE="${SOURCE_DIR}/package"

mkdir -p "${USER_CONFIG_PATH}" "${EMACS_CONFIG_PATH}"

# Sync the entire 'package' directory
rsync -av --delete "${SOURCE_PACKAGE}/" "${USER_CONFIG_PATH}/"

SUFFIX=.template
# Copy each template, stripping the '.template' suffix
for TEMPLATE_PATH in "${SOURCE_TEMPLATES}"/*${SUFFIX}; do
    [[ -e "$TEMPLATE_PATH" ]] || continue  # skip if no matching files
    dest_file="${EMACS_CONFIG_PATH}/$(basename "$TEMPLATE_PATH" ${SUFFIX})"
    cp -v "$TEMPLATE_PATH" "$dest_file"
done

echo "Host config updated successfully."

