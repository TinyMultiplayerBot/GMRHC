Giant Multiplayer Robot Haskell Client
======================================

A simple commandline tool for downloading and uploading Civilization V saves to Giant Multiplayer Robot

Dependencies
------------

- GHC
- cabal
- An Authentication Key from http://multiplayerrobot.com/Download


Build
-----

    cabal install -j


Use Instructions
----------------

- Download all games

    GMRHC --key &lt;Authentication Key>

- List your current Games and Turn IDs

    GMRHC --key &lt;Authentication Key> --games

 - NOTE: Turn IDs are currently needed to upload saves

- Upload a Save

    GMRHC --key &lt;Authentication Key> --upload &lt;Turn ID> &lt;Save File Path>
