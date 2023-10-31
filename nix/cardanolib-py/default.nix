{ python3Packages }:

python3Packages.buildPythonPackage {
  version = "1.0.0";
  pname = "cardano-lib-py";
  src = ./.;
}
