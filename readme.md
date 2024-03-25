# HAMR System Testing Artifact Generator

## How to Use

```
git clone https://github.com/santoslab/hamr-system-testing-artifact-generator.git

cd hamr-system-testing-artifact-generator

# build a runnable jar
sireum proyek assemble --uber -m org.sireum.hamr.systest.generator.SystemTestArtifactGen .

# optionally create an alias to the runnable jar
alias sysgen=$(pwd)/out/hamr-system-testing-artifact-generator/assemble/hamr-system-testing-artifact-generator.jar.bat

# run the generator by passing it the root of proyek project and paths to one or more files that contain
# input/output container definitions
sysgen isolette/hamr/slang src/main/util/isolette/system_tests/rst/Regulate_Subsystem_Containers.scala
```