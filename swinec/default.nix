{ mkDerivation
, aeson
, base
, hashable
, lens
, mtl
, text
, transformers
, unordered-containers }:
mkDerivation {
    # That's what it's called!
    pname = "swinec";

    # Keep the version in sync with the one in swinec.cabal.
    version = "0.0.0.0";

    # For now, just set this to null. We have to set it to something.
    license = null;

    # Otherwise it'll try to download it from Hackage.
    src = ./.;

    # Keep the dependencies in sync with those in swinec.cabal.
    buildDepends = [
        aeson
        base
        hashable
        lens
        mtl
        text
        transformers
        unordered-containers
    ];
}
