GMRHC -  Giant Multiplayer Robot Haskell Client
===============================================

A simple commandline tool for downloading and uploading Civilization V saves to Giant Multiplayer Robot

Dependencies
------------

- GHC
- cabal


Build
-----

    cabal build


Use Instructions
----------------

- Download all games

    GMRHC --key <API KEY>

- List your current Games and Turn IDs

    GMRHC --key <API KEY> --games

 - NOTE: Turn IDs are currently needed to upload saves

- Upload a Save

    GMRHC --key <API KEY> --upload <Turn ID> <Save File Path>
