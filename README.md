# haskell-book

My solutions to the *Haskell Book* exercises.

## Using the repo
This repo can be *"run"* with GitPod. [![Gitpod Ready-to-Code](https://img.shields.io/badge/Gitpod-Ready--to--Code-blue?logo=gitpod)](https://gitpod.io/from-referrer/)

When opening an Haskell file for the first time VSCode will ask:

    How do you want the extension to manage/discover HLS d the relevant toolchain?

Select `Manually via PATH`

The pod comes with preinstalled:
- ghc 9
- stack
- hls

## Repo structure

Each chapter has its dedicated folder containing a dedicated stack package.

The `src` folder contains the code created for the chapter's exercises while the `test` folder contains the test suite.

## Running the code

To run the code just open a terminal and enter:
- `stack test` to run all chapter's tests
- `stack test chapterXX` to run only the test for chapter XX