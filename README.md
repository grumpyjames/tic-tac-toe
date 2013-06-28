Using QuickCheck to drive design - an experiment
------------------------------------------------

Code resulting from the [Property Based TDD workshop](http://natpryce.com/articles/000802.html) at [SPA 2013](http://spaconference.org)

#### Notes

* Only the second time I've used QuickCheck; code heavily inspired by examples on Stack Overflow, and some heavy borrowing from [this repo](https://github.com/jonathanperret/spa2013-property-based-testing). Please don't use this code as a shining example to be borrowed from just yet, it's probably heinously non-idiomatic.
* There's probably more value in examining how the code evolved rather than just looking at HEAD (there are some amusing mis-steps in the history, for a start)

#### Discoveries

* The best properties are those that have implementation different from their specification. Checking `isInfixOf` for both `Oh` and `Ex` involves examining the board twice; the `foldr` approach used avoids this.
* Very specific properties might result in QuickCheck giving up on you. That's by no means a bad thing - get to the point where your implementation passes all the tests until QC gives up, and then try a more generic property.

#### Further work

* Still needs to work out who won, if there is a result.
* Extending the scorer for an `n` by `n` game might lead to more fun.
