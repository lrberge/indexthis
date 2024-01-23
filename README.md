
# indexthis: fast index creation

The only purpose of this package is to create indexes (aka group IDs, aka keys) as fast as possible. Indexing turns a vector, or group of vectors of the same length, to a single *integer* vector ranging from 1 to the number of unique values in the vector, or group of vectors.

Indexes are important inputs to many algorithms (usually low level). Whiile working on the original vectors can be slow, working with a simple integer vector with nice properties (unitary increments) is fast. And you can always retrieve the original vectors from the index.

But enough talk, here's a simple example:
```R
x = c("u", "a", "a", "s", "u", "u")
to_index(x)
# [1] 1 2 2 3 1 1

y = c(  5,   5,   5,   3,   3,   7)
to_index(x, y)
#> [1] 1 2 2 3 4 5
```

As you can see, the vector `x` is turned into an integer whose values map the unique values of `x`. The second example is the same for the combination of `x` and `y`. This function is equivalent to, and is inspired from, [collapse::GRPid](https://sebkrantz.github.io/collapse/reference/GRP.html).

## How does it work?

The algorithm to create the indexes is based on a partial-hashing of the vectors in input. 

A table of size `2 * n + 1`, with `n` the number of observations, contains the index (observation id) of the first value with that hash. Hence each hash value must lie within `[0; 2*n]`, leading to a *de facto* partial hashing.

All the values of the input vector are turned into a 32 bits hash, which is itself turned into a `log2(2 * n)` bits hash simply by shifting the bits. When there are multiple vectors in input their hash values are simply xor'ed to create a single hash.

The fact that hash values must lie within `[0; 2*n]` necessarily leads to multiple collisions (i.e. different values leading to the same hash). This is why collisions are checked systematically, guaranteeing the validity of the resulting index.

Although the algorithm may look slow at first sight because of the extensive collision checking, in practice it works very well. 

This algorithm is brilliant and is faster than everything else I've tested. The idea definitely isn't mine. I've got it from peering into Sebastian Krantz's [`collapse`](https://sebkrantz.github.io/collapse/index.html) (excellent) package, who's got it from Morgan Jacob's [`kit`](https://github.com/2005m/kit) package. Morality: OSS rocks.

## Difference with existing algorithms

This algorithm departs from `collapse`'s implementation quite heavily. In fact there exists faster algorithms for special cases: when the data looks like integers. In particular, the difference in speed should be seen for multiple vectors. There are tricks to combine input vectors that look like integers (even without being integers) and I tried hard to take advantage of this (that part is original).

## Why this package?

As mentionned in the introduction, indexes are a key input to many algorithms. Creating a 0-dependencies (except Rcpp) package dedicated to this key step makes sense from a programming perspective. Ideally, having such a functionnality in base R would be the best, but this is a second best.

Note that since it is based on a single `cpp` file, if you want to use it in one of your projects, it is easy to just copy the file directly into your project, guaranteeing stability.



