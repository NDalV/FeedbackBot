{
  lib,
  stdenvNoCC,
  python3,
  writeText,
  mkShell,
}: {
  name,
  src,
  pythonPackages ? _ps: [],
  packages ? [],
  config ? null,
  configFile ? null,
}: let
  configAsYAML = writeText "mkdocs.yml" (lib.generators.toYAML {} config);
  finalConfig =
    if configFile == null
    then configAsYAML
    else configFile;
  packagesFinal =
    packages
    ++ [
      (python3.withPackages (ps: [ps.mkdocs] ++ pythonPackages ps))
    ];
in {
  docs = stdenvNoCC.mkDerivation {
    inherit src;
    name = "${name}-docs";

    nativeBuildInputs = packagesFinal;

    unpackPhase = ''
      runHook preUnpack
      cp -r $src docs
      runHook postUnpack
    '';
    buildPhase = ''
      cp ${finalConfig} mkdocs.yml
      mkdocs build
    '';
    installPhase = ''
      mkdir -p $out/doc
      cp -r site $out/doc
    '';
  };
  env = let
    shellHook =
      if configFile == null
      then ''
        ln -sf ${finalConfig} mkdocs.yml
      ''
      else finalConfig;
  in
    mkShell {
      packages = packagesFinal;
      inherit shellHook;
    };
}
