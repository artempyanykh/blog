# Blog in Hakyll

## How to update package versions to latest LTS

Stackage exposes cabal.config with a list of constraints to use for the resolver:
```
curl https://www.stackage.org/{lts-N}/cabal.config | tee cabal.project.freeze > /dev/null
```