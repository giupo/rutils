Changelog
=========


v0.10.2 (2025-04-24)
--------------------
- Parametrize R&Rscript in Makefile. [MasterOfPuppets]


v0.10.1 (2025-01-08)
--------------------
- Fixed check failing. [Giuseppe Acito]
- Makes a more thoughtful test for is_r_project. [Giuseppe Acito]


v0.10.0 (2025-01-08)
--------------------
- Adds is_r_project. [Giuseppe Acito]


v0.9.20 (2025-01-07)
--------------------
- Removes lock file after releaseing it. [Giuseppe Acito]


v0.9.17 (2024-06-17)
--------------------
- Updated docs. [Giuseppe Acito]


v0.9.16 (2024-06-17)
--------------------
- Updated prev_quarter to a decent implementation. [Giuseppe Acito]


v0.9.13 (2024-01-04)
--------------------

Fix
~~~
- Removes unused code. [Giuseppe Acito]


v0.9.12 (2024-01-04)
--------------------
- Pkg: usr: evaluate prev_quarter based on day of the year. [Giuseppe
  Acito]


v0.9.11 (2022-11-04)
--------------------
- Updates to work on 4.2.0. [Giuseppe Acito]
- Fixes tests on 4.2.0, by removing a test. [Giuseppe Acito]


v0.9.10 (2022-09-15)
--------------------
- Updates DESCRIPTION. [Giuseppe Acito]


v0.9.9 (2022-08-05)
-------------------
- Moved tests from rcf for check_multi_core. [giupo]


v0.9.8 (2022-08-05)
-------------------
- R CMD checks OK. [giupo]
- Updates CHANGELOG.md. [giupo]


v0.9.7 (2022-08-05)
-------------------
- Updates CHANGELOG.md. [giupo]
- Updates docs. [giupo]
- Adds prev_quarter from rcf. [giupo]
- Updates CHANGELOG. [giupo]


v0.9.6 (2022-08-04)
-------------------
- Fix messed up message. [giupo]


v0.9.5 (2022-08-04)
-------------------
- Update NAMESPACE. [giupo]


v0.9.4 (2022-08-04)
-------------------
- Move check_multicore to rutils. [giupo]
- Updates CHANGELOG.md. [giupo]


v0.9.3 (2022-05-24)
-------------------
- Updates and fixes some docs. [giupo]
- Fixes checks warnings. [giupo]
- Trying to fix docs on R6. [giupo]
- Updates docs for Stack. [giupo]
- Nothing important. [giupo]


v0.9.2 (2022-04-20)
-------------------
- Adds TOML parser. [giupo]


v0.9.1 (2022-04-20)
-------------------
- Fix wrong call level. [giupo]
- Fix logging not working with params. [giupo]
- Minor changes for lintr. [giupo]


v0.9.0 (2022-04-15)
-------------------
- Adds logging configuration. [giupo]
- Merge branch 'feature/addAppender' into develop. [giupo]
- 100 coverage, can commit. [giupo]
- Remove require from tests in favour of requireNamespace. [giupo]
- Adds appender, remove ini_parse (delegates to configr) [giupo]
- Adds .gitlab-ci.yml to Rbuildignore. [giupo]
- Update .gitlab-ci.yml. [MasterOfPuppets]
- Adds Gitlab CI. [giupo]
- Adds minor stuff. [giupo]


v0.7.5 (2021-06-17)
-------------------
- Completed code coverage. [giupo]
- Updates to testthat 3. [giupo]


v0.7.4 (2021-04-09)
-------------------
- Adds ifelse. [giupo]


v0.7.3 (2020-03-17)
-------------------
- Fuck you windows. [Giuseppe Acito]
- Adds if in .Rprofile to bypass renv. [Giuseppe Acito]
- Update renv; adds encoding in DESCRIPTION. [Giuseppe Acito]


v0.7.2 (2019-11-19)
-------------------
- Moving to renv. [Giuseppe Acito]
- Remove deps on tk. [Giuseppe]


v0.7.1 (2019-11-06)
-------------------
- Remove XML from DESCRIPTION. [giupo]
- Updates roxygen version in DESCRIPTION. [Giuseppe Acito]


v0.6.1 (2019-07-30)
-------------------
- Stupid tests on time. [Giuseppe Acito]


v0.6.3 (2019-04-04)
-------------------
- Merge branch 'hotfix/removeXML' [Giuseppe Acito]
- Removed XML. [Giuseppe Acito]


v0.6.2 (2019-04-03)
-------------------
- Remove XML from DESCRIPTION. [Giuseppe Acito]
- Refresh packrat. [Giuseppe Acito]


v0.6.0 (2019-03-19)
-------------------
- Remove internal flypwd. [Giuseppe Acito]


v0.5.3 (2018-10-01)
-------------------
- Adds --vanilla to ignore local init.R for packrat. [Giuseppe Acito]
- Minor diffs. [Giuseppe Acito]


v0.5.2 (2018-10-01)
-------------------
- Adds invisible to password output of flypwd. [Giuseppe Acito]


v0.5.1 (2018-07-04)
-------------------
- Added .Rprofile for packrat; moved from 'Depends:' to 'Imports:' in
  DESCRIPTION. [Giuseppe Acito]
- Moved Depends to Imports to less cluttering in namespace. [Giuseppe
  Acito]
- Testthat downgraded to 2.0.0 from 2.0.0.9000. [Giuseppe Acito]
- Snapshotted to 3.2.3. [Giuseppe Acito]
- Added needed files under packrat/ [Giuseppe Acito]
- Removed references to packrat. [Giuseppe Acito]
- Recovered original .gitignore. [Giuseppe Acito]
- Updated for testthat 2.0 e R-3.3.3. [Giuseppe Acito]
- Updated packrat. [Giuseppe Acito]
- Updated packrat. [Giuseppe Acito]
- Adds sep for show. [Giuseppe Acito]
- Minor glitches. [Giuseppe Acito]
- Adds is.Stack to NAMESPACE; adds show method to Stack. [Giuseppe
  Acito]
- Adds Stack to Namespace. [Giuseppe Acito]
- Adds Stack. [Giuseppe Acito]
- Bad bad bad copy/paste. [Giuseppe]
- Added pretty empty and void README.md. [Giuseppe Acito]
- Removed cluster code. [Giuseppe Acito]
- Good coverage, must complete cluster. [Giuseppe Acito]
- Updated some tests. [Giuseppe Acito]
- Updated some tests. [Giuseppe Acito]
- Refactor di .basename: getting rid of bullshit. [Giuseppe Acito]
- Updated .travis.yml. [Giuseppe Acito]
- Maledizione: era in ignore 'man' [Giuseppe Acito]
- I dunno what's wrong here for Travis. [Giuseppe Acito]
- Added proper mocking of flypwd in tests. [Giuseppe Acito]
- Check and tests ok. Still issues with flypwd mocking. [Giuseppe Acito]
- Prepare to complete check rounds. [Giuseppe Acito]
- Add stub .travis.yml. [Giuseppe Acito]
- Update packrat.lock. [Giuseppe Acito]
- Cambiato JunitReporter. [Giuseppe Acito]
- Ignora i files di packrat. [Giuseppe Acito]
- Rimosso testthat da Suggests: era gia' in Depends. [Giuseppe Acito]
- Fix some minor issues on docs and params of docs. [Giuseppe Acito]
- Typo nel DESCRIPTION. [Giuseppe Acito]
- Merged. [Giuseppe Acito]
- Merged. [Giuseppe Acito]
- Testing ok. [Giuseppe Acito]
- Updated packages. [Giuseppe Acito]
- Packrat added. [Giuseppe Acito]
- Merge branch 'tomerge' [Giuseppe Acito]
- Better check for jenkins. [Giuseppe Acito]
- Better check for jenkins. [Giuseppe Acito]
- Better check for jenkins. [Giuseppe Acito]
- Better check for jenkins. [Giuseppe Acito]
- Better check for jenkins. [Giuseppe Acito]
- Better check for jenkins. [Giuseppe Acito]
- Typo in test_check. [Giuseppe Acito]
- Typo in JunitReporter. [Giuseppe Acito]
- Aggiungto suggests R6. [Giuseppe Acito]
- Passo a R6 e JunitReporter trovato su
  https://github.com/lbartnik/testthat/blob/junit-xml2/R/reporter-
  junit.R. [Giuseppe Acito]
- Major overhaul. [Giuseppe Acito]
- Trying to figure out Jenkins. [Giuseppe Acito]
- Push to OSX. [Giuseppe Acito]


v0.2.3 (2015-11-03)
-------------------
- Fix bug in Cluster. [Giuseppe Acito]


v0.2.2 (2015-11-03)
-------------------
- Fix bug on Cluster. [Giuseppe Acito]
- Odio tutti. [Giuseppe Acito]
- Aggiunto override di readLines. [Giuseppe Acito]
- Fix #1. minor bullshit. [Giuseppe Acito]
- Prima di git flow. [Giuseppe Acito]
- Sposta ini_parse da rcf a qui. [Giuseppe Acito]
- Prepull. [Giuseppe Acito]


v0.0.7 (2015-09-25)
-------------------
- Aggiunge anche NOTIFY all'export. [Giuseppe Acito]
- Riaggiungo notify per usi futuri. [Giuseppe Acito]


v0.0.6 (2015-09-23)
-------------------
- Nothing important. [Giuseppe Acito]


v0.0.5 (2015-09-23)
-------------------
- Some minor improvements. [Giuseppe Acito]


v0.0.4 (2015-09-23)
-------------------
- Aggiunge .randomString e .containsString al NAMESPACE. [Giuseppe
  Acito]
- Add some methods to Cluster. [Giuseppe Acito]
- Let'see if it works this way. [Giuseppe Acito]
- Initial Commit. [Giuseppe Acito]
- Initial commit. [Giuseppe Acito]


