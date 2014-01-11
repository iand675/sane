# Sane
## The simple, beautiful way to remember anything.

Oft-repeated procedures are noted here for quick reference.

### Building and Emulating the Phonegap Application

Ensure Bower, Grunt, and Phonegap packages are installed globally. Suitable only for Mac environments with XCode.

1. cd into the "mobile" directory.
2. npm install
3. grunt init
4. grunt emulate

### Running server

Install vagrant & ansible. Base OS shouldn't matter too much.

Initializing the server for the first time (from the base project directory):

0. `vagrant provision`
1. `vagrant up`
2. `vagrant ssh`
3. `cd /vagrant/server/web`
4. `cabal sandbox init`
5. `cabal install --only-dependencies`

Building && running the server:

0. `vagrant ssh`
1. `cd /vagrant/server/web`
2. `cabal build`
3. `./dist/build/sane/sane`
4. Server will be running at 192.168.33.10:3000/
