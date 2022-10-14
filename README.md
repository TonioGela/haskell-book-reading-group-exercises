# haskell-book

My solutions to the *Haskell Book* exercises.

## Using the repo
This repo can be *"run"* with GitPod. [![Open in Gitpod](https://gitpod.io/button/open-in-gitpod.svg)](https://gitpod.io/#https://github.com/nivox/haskell-book)

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