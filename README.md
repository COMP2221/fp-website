Source repository for the functional programming part of
[COMP2221](https://www.dur.ac.uk/faculty.handbook/module_description/?year=2021&module_code=COMP2221).

FIXME: Add link to hosted material once it is up and running

The .github/workflows/build.yml shows how the material is built. You will need [hugo](https://gohugo.io).

Github actions are set up so that any push to the main branch is automatically built and deployed. I recommend making changes via pull requests which check that everything still builds which one can then merge. At present these are switched off (enable them in the repo settings once the hosting stuff is sorted out).

All the live code is set up to use [stack](https://www.haskellstack.org), if you add a new directory for live code in the lectures, then `package.yaml` and `hie.yaml` will probably need to be edited.
