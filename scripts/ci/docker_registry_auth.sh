#!/bin/sh
set -eu

current_dir=$(cd "$(dirname "${0}")" && pwd)

# Setup Docker registry authentication for either Docker Hub (release) or GitLab registry (dev).
# Also setup Docker names such that they are valid for the target (Docker Hub or GitLab).
# Docker constraints on tags:
# https://docs.docker.com/engine/reference/commandline/tag/

#  A tag name must be valid ASCII and may contain lowercase and
#  uppercase letters, digits, underscores, periods and dashes. A tag
#  name may not start with a period or a dash and may contain a maximum
#  of 128 characters.

# GitLab constraints on images:
# https://docs.gitlab.com/ee/user/packages/container_registry/#image-naming-convention

# Create directory for Docker JSON configuration (if does not exist)
mkdir -pv ~/.docker

# /!\ MASTER_OR_RELEASE can be unset
if [ "${MASTER_OR_RELEASE:-}" = 'true' ] && [ "${TEZOS_DEFAULT_NAMESPACE}/${TEZOS_DEFAULT_BRANCH}" = 'tezos/tezos' ]
then
  docker_image_name="docker.io/${CI_PROJECT_PATH}-"
  echo "{\"auths\":{\"https://index.docker.io/v1/\":{\"auth\":\"${CI_DOCKER_AUTH}\"}}}" > ~/.docker/config.json
else
  docker login -u "${CI_REGISTRY_USER}" -p "${CI_REGISTRY_PASSWORD}" "${CI_REGISTRY}"
  docker_image_name="${CI_REGISTRY}/${CI_PROJECT_NAMESPACE}/${CI_PROJECT_NAME}/"
fi

# /!\ IMAGE_ARCH_PREFIX can be unset
docker_image_tag=$(echo "${IMAGE_ARCH_PREFIX:-}${CI_COMMIT_REF_NAME}" | tr -c -- '-._\n[:alnum:]' '_')

## Write computed Docker environment variables to sourceable file for other shell scripts

echo "export DOCKER_IMAGE_NAME=${docker_image_name}" > "${current_dir}/docker.env"
echo "export DOCKER_IMAGE_TAG=${docker_image_tag}"  >> "${current_dir}/docker.env"

## Output (debug)

# shellcheck source=./scripts/ci/docker.env
. "${current_dir}/docker.env"
# shellcheck source=./scripts/ci/docker.sh
. "${current_dir}/docker.sh"

echo '### Docker image names:'

echo "${docker_build_image}:${DOCKER_IMAGE_TAG}"

for docker_image in ${docker_images}
do
  echo "${docker_image}:${DOCKER_IMAGE_TAG}"
done
