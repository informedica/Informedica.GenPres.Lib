# Informedica.GenPres.Lib
Library for calculation of medical orders. This library lets you 
define a medical order and apply constraints. These constraints will
be applied each at a time. Upon applying a constraint, the resulting 
set of options is calculated with resulting constraints.

The current solution file contains a number of projects. These projects 
will move to separate repositories and will be referenced by Nuget. For
ease of developing, these are currently bundled together.

## Builds

GitHub Actions |
:---: |
[![GitHub Actions](https://github.com/Informedica/Informedica.GenPres.Lib/workflows/Build%20master/badge.svg)](https://github.com/informedica/Informedica.GenPres.Lib/actions?query=branch%3Amaster) |
[![Build History](https://buildstats.info/github/chart/Informedica/Informedica.GenPres.Lib)](https://github.com/informedica/Informedica.GenPres.Lib/actions?query=branch%3Amaster) |

## NuGet 

No Nuget packages yet.

Package | Stable | Prerelease
--- | --- | ---
Informedica.GenPres.Lib | [![NuGet Badge](https://buildstats.info/nuget/Informedica.GenPres.Lib)](https://www.nuget.org/packages/Informedica.GenPres.Lib/) | [![NuGet Badge](https://buildstats.info/nuget/Informedica.GenPres.Lib?includePreReleases=true)](https://www.nuget.org/packages/Informedica.GenPres.Lib/)

---

### Developing

How to develop and contribute is described at: https://informedica.nl/?p=320

The general principle is that you can run, test and develop all code in 
the FSI. 

To contribute, please fork this repository and make pull requests.

---


### Building

Building is kept deliberately simple. You can use either the build 
commands or use

```
dotnet tool restore
dotnet build
dotnet test
```

---



### Git push tags

Use the following tags for pushing changes to git:

- release
- feat: new feature
- fix: fix a bug or problem
- docs: document
- refactor: refactoring
- perf: improve performance
- test: add test
- chore: do a chore (build, libs, etc..)
