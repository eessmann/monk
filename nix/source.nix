{ lib }:
lib.cleanSourceWith {
  src = ../.;
  name = "monk";
  filter = path: type:
    let name = baseNameOf path; in
    lib.cleanSourceFilter path type
    && !(builtins.elem name [ ".devenv" ".direnv" ".superpowers" "artifacts" "dist-newstyle" ".hie" "__pycache__" "cabal.project.local" "result" "target" ])
    && !(lib.hasPrefix "result-" name)
    && !(type == "directory" && (name == "dist" || lib.hasPrefix "dist-" name));
}
