set -x
set -e

cp ../backend/package.json ./backend/
cp ../backend/package-lock.json ./backend/
node2nix -d -i ../backend/package.json -l ../backend/package-lock.json -o backend/node-packages.nix -c ./backend/op-energy-backend.nix -e ./backend/node-env.nix

cp ../frontend/package.json ./frontend/
cp ../frontend/package-lock.json ./frontend/
node2nix -d -i ../frontend/package.json -l ../frontend/package-lock.json -o frontend/node-packages.nix -c ./frontend/op-energy-frontend.nix -e ./frontend/node-env.nix

cat node-env.patch | patch -p1
sed -i 's/sources."nice-napi-1.0.2"/(sources."nice-napi-1.0.2" \/\/ { dependencies = [ sources."node-gyp-build-4.3.0" ]; } )/' ./frontend/node-packages.nix
