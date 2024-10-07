_: {
  perSystem = {
    pkgs,
    config,
    ...
  }: let
    mkDocs = pkgs.callPackage ./mkdocs.nix {};
    shimmerDocs = mkDocs {
      name = "FeedbackBot";
      src = ../docs;
      pythonPackages = ps: [ps.plantuml-markdown];
      packages = [pkgs.plantuml];
      config = {
        site_name = "Shimmer";
        plugins = ["search"];
        markdown_extensions = ["plantuml_markdown"];
        theme = "readthedocs";
      };
    };
  in {
    packages.docs = shimmerDocs.docs;
    devShells.docs = shimmerDocs.env;
  };
}
