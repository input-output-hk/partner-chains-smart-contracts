{
  repoRoot,
  inputs,
  pkgs,
  lib,
  system,
  ...
}: {
  patchedProject = args: let
    project = pkgs.purescriptProject args;
    project' = project.compiled.overrideAttrs (old: {
      # Add a patchPhase for patching in the CLI version from a script
      patchPhase = let
        rev =
          if builtins.hasAttr "rev" inputs.self
          then inputs.self.rev
          else inputs.self.dirtyRev;
      in ''
        chmod -R +w src
        substituteInPlace src/set_version.sh \
          --replace 'jq' '${pkgs.jq}/bin/jq'
        sed -i 's/gitHash=".*"/gitHash="${rev}"/' src/set_version.sh
        pushd src
        ${pkgs.bash}/bin/bash set_version.sh
        popd
    '';
    });
  in
    project //
    { compiled = project'; };
}
