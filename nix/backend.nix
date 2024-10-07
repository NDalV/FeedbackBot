{
  inputs,
  self,
  ...
}: {
  perSystem = {
    self',
    system,
    lib,
    pkgs,
    config,
    ...
  }: {
    _module.args.pkgs = import self.inputs.nixpkgs {
      inherit system;
      config.allowUnfree = true;
    };
    haskellProjects.default = {
      autoWire = ["packages" "checks"];

      basePackages = pkgs.haskellPackages.override {
        inherit (inputs) all-cabal-hashes;
      };
      packages = {
        mmzk-typeid.source = "0.7.0.0";
        effectful.source = "2.3.1.0";
        effectful-core.source = "2.3.1.0";
      };
      settings = let
        unbrokeUncheck = {
          broken = false; # mark unbroken
          check = false; # dont run tests
        };
      in
        {
          # we depend on swagger2 because getshoptv protocol lib depends on it (why?), and tests fail with non-ordered keymap
          swagger2.check = false;
          # failed tests with non-ordered keymap
          lsp-types.check = false;
          # for memory/performance reasons
          aeson.cabalFlags.ordered-keymap = false;
          mmzk-typeid = unbrokeUncheck // {jailbreak = true;};
          feedback-bot = {self, ...}: {
            justStaticExecutables = true;
            cabalFlags.production = true;
            custom = prev: self.forceLlvmCodegenBackend prev;
          };
          growable-vector.broken = false;
        }
        // lib.attrsets.genAttrs ["avro" "singletons-base" "records-sop" "servant-effectful"] (_pkg: unbrokeUncheck);
    };
    pre-commit.settings.hooks = {
      fourmolu.enable = true;
      cabal-fmt.enable = true;
    };
    packages = {
      default = self'.packages.feedback-bot;
    };
  };
}
