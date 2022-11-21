Stress tests for messenger service
==============================================

We use [k6](https://k6.io/) to run stress tests on servers.
By default we use 20 users and 60 sec time.
See makefile variable `options` to tweak it.
All test cases are run on the same scripts only routes are a bit different.
Note that we should restart service prior to any test for fare comparison.

To run tests use `make run` with variables:

* `logs`: use `false` to turn off logging, default value is `true`
* `method`: api method to call repeatedly. It can be `save`, `get-id` or `get-tag`.

Example:

Run server with logging turned off and on method `save`:

```
make logs=false method=save run
```
