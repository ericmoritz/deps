__Warning: deps is in active development; don't use until 1.0 is released__

deps
=======

A general purpose dependency management tool. It is built from the unix philosophy:
   
> Write programs that do one thing and do it well. Write programs to
> work together. Write programs to handle text streams, because that
> is a universal interface.

The goal of deps is to do one thing and do it well.  The result of its
work will be used by your build tools of choice to build your project.
`deps` only job is to take a list of URLs and download them into a
directory; that's it.

Design
-------

All `deps` does is copy files from some remote location and stick them
in a directory in `deps/`. It does that using a file called
`deps.txt`.

The format of `deps.txt` is extremely simple:

```
deps     git://eric@themoritzfamily.com/~/eric/git/deps.git#develop
nanoweb  https://pypi.python.org/packages/source/n/nanoweb/nanoweb-1.0.tar.gz
wand-0.1 ~/projects/wand-0.1
```

As you can see it is just a list of URLs with a name.

Using this `deps.txt` file, the following directory structure will be
created:

```
deps/
     deptool/
     nanoweb/
     wand-0.1/
```

Algorithm
----------

The `deps` algorithm is to be as explicit as possible on how `deps` works:

1. Enqueue urls from ./deps.txt
2. pop a url from the queue
3. If deps/{name} does not exist, download url to deps/{name}
4. Equeue urls from deps/{name}/deps.txt
5. goto 2 until queue is empty

This algorithm has the following properties:

1. The dependancies are processed
a [breadth first](https://en.wikipedia.org/wiki/File:Animated_BFS.gif) order
2. A dependancy that comes before a duplicate dependancy takes presedence

Why
----

So, why did we choose these properties?  We had two requirements for
our dependancy management system.  

First, during development of a product that is composed of multiple
packages, we needed to have a persistant copy of dependancies while
working on a feature. That is why `deps` will never clobber an already
existing dependancy in `deps/`.

Second, we needed to place the decision of version conflicts in the
hands of a parent dependancy.  

If two projects have conflicting dependancies, a parent should be able
to choose a version that is compatible with both rather that crash the
build.
