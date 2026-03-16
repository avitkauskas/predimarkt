#!/usr/bin/env bash
set -e

SERVER="root@snookerscore.club"
IMAGE_NAME="ihp-app"
CONTAINER_NAME="predimarkt"
NETWORK="predimarkt-net"

echo "==> Building image in OrbStack amd64 VM..."
orb -m ihp-builder-amd64 -u alvis bash -c "
  cd /Users/alvis/code/haskell/predimarkt &&
  nix build .#optimized-docker-image --max-jobs 1 --cores 1 &&
  cp -f \$(readlink result) /Users/alvis/ihp-app.tar.gz
"

echo "==> Copying image to server..."
scp ~/ihp-app.tar.gz $SERVER:/root/ihp-app.tar.gz

echo "==> Deploying on server..."
ssh $SERVER bash <<'EOF'
  set -e
  echo "Loading image..."
  NEW_IMAGE=$(docker load < /root/ihp-app.tar.gz | grep "Loaded image:" | awk '{print $3}')
  echo "Loaded: $NEW_IMAGE"

  echo "Stopping old container..."
  docker rm -f predimarkt || true

  echo "Starting new container..."
  docker run -d \
    --name predimarkt \
    --restart unless-stopped \
    --env-file /root/.env \
    --network predimarkt-net \
    $NEW_IMAGE

  echo "Checking health..."
  sleep 3
  docker logs predimarkt
  docker ps | grep predimarkt
  rm -f /root/ihp-app.tar.gz
EOF

echo "==> Done! Cleaning up local image..."
rm -f ~/ihp-app.tar.gz
echo "==> Deployed successfully!"