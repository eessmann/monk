{ referencePkgs }:
{
  # Use the upstream packages from locked nixpkgs inputs, including their normal
  # platform configuration. Do not maintain a private Bash source build.
  bash = assert referencePkgs.bashNonInteractive.version == "5.3p9"; referencePkgs.bashNonInteractive;
  fish = assert referencePkgs.fish.version == "4.6.0"; referencePkgs.fish;
}
