# HTHttpd

HTHttpd is a small, RFC2616-ignorant, incomplete web server written in Haskell. It's purpose is not to supplant any of the existing Haskell web servers such as Warp, but rather to act as a teaching tool. Two versions have been created (emphasis on the repository you're currently browsing):

1. [HTHttpd-bad](https://github.com/alexwestholm/HTHttpd-bad): purposefully written in non-idiomatic Haskell similar to what a developer new to statically-typed functional programming might create; and
2. **[HTHttpd](https://github.com/alexwestholm/HTHttpd)**: a more idiomatic version of the same functionality. This is intended to help green Haskell developers develop a sense for how to use the language, compiler and type-checker to produce higher quality, less buggy code.

### Contributing 

This is intended to be an exercise in writing *better* Haskell. The HTHttpd-bad version is pretty unimpressive, and easy to improve on. However, there is no such thing as perfect code (even in Haskell!), so HTHttpd can always be made better from both a code and functionality standpoint. How can your pull requests best aid the goal of teaching idiomatic Haskell?

1. If you notice areas of the code that need improvement, annotate them! There are things I'd love to do but haven't had time for peppered throughout the code; in each case, I've tried to indicate that this might be a good place for improvement. If you see something, say something.
2. As you refactor HTHttpd, please document what you're doing in the code. Explain the weakness of the original approach, enumerate alternatives, and argue the superiority of your chosen path.
3. If you contribute new functionality to HTHttpd (such as enhanced RFC compliance, or additional parsing of request bodies for non-GET HTTP methods... hint, hint), please also add that functionality to HTHttpd-bad. But *please* try to do so in a non-idiomatic, bad-Haskell manner (and document why it's bad).

### Building

HTHttpd makes use of the Haskell Platform, please install that first. From there:

```sh
$ cabal configure; cabal install
$ cabal run
$ curl localhost:31337/cool
```

#### License
MIT

