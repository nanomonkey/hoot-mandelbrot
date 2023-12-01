# Guile Hoot FFI demos

This repository includes a few demos that use Hoot's FFI to interact
with the browser DOM API.

## How to run the demos

This is a bleeding edge demo that requires a lot of very fresh
software.  GNU Guile built from the main branch, Guile Hoot built from
the main branch, and a very recent browser such as Chrome 119 or
Firefox 121.

You need to get the browser on your own, but for getting a working
Guile and Hoot we have provided a [Guix](https://guix.gnu.org)
manifest file that can be used to setup the environment.  Once Guix is
installed, just run:

```
guix shell -m manifest.scm
```

Guile in particular takes a long time to build so expect to wait
awhile.

Once your environment is up, the examples can be built like so:

```
make
```

Then launch a simple web server:

```
make serve
```

Once the web server is up, visit http://localhost:8088 and click on a
link to the demo you'd like to check out.

## License

All code in this repository is licensed under Apache 2.0, expect for
`web-server.scm` which is borrowed from
[Haunt](https://dthompson.us/projects/haunt.html) and is under the
GPLv3 license.
