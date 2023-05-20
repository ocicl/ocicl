# ocicl
> An ASDF system distribution and management tool for Common Lisp

What is it?
------------
``ocicl`` is a modern alternative to quicklisp.  It is modern in the sense that:
* all software is [bundled as OCI-compliant artifacts](https://oras.land/) and distributed from an OCI-compliant registry (the github container registry).
* all software is distributed over secure (TLS) connections.
* [sigstore](https://www.sigstore.dev/) tooling is used to ensure the integrity and authenticity of all software bundles.
* all software bundles are project-local, making it easy to lock specific versions to your own projects.
* all software bundles are built and published transparently using hosted CI infrastructure ([github actions](https://github.com/ocicl/ocicl-action)).

``ocicl`` is pronounced like
"[ossicle](https://en.wikipedia.org/wiki/Ossicles)", a tiny bone
embedded in your middle ear.  Like the ossicles in your ear, the
``ocicl-runtime`` is a tiny library that is embedded in your lisp
image.  It is responsible for finding and loading
[ASDF](https://asdf.common-lisp.dev/) systems that you manage with the
``ocicl`` command line tool.

The main innovation behind ``ocicl`` is the idea of applying the
ecosystem of tooling and services from the world of application
container images to ordinary tarballs of Lisp code. OCI + CL = ``ocicl``.

``ocicl`` is under active development.  It currently requires SBCL and
only works on Linux, but adapting to other systems and platforms
should not be difficult.  Feedback is welcome at
https://github.com/ocicl/ocicl/issues.  Pull requests are even more welcome, at https://github.com/ocicl/ocicl/pulls!

Quick Start
------------

Install ``ocicl`` by running ``make`` in the source directory.  This
will build the ``ocicl`` binary and install it in ``${DESTDIR}/bin``.
It will also install a helper program called ``ocicl-oras``.  The
default value for ``DESTDIR`` is ``${HOME}/.local/bin``, but you can
change it at install time like so:
```
$ DESTDIR=/usr/local make install
```
Make sure the ``${DESTDIR}/bin`` directory is on your path.

Now run ``ocicl setup``.  This is a mandatory step that installs the
``ocicl-runtime`` library, and suggests configurations for your
``${HOME}/.sbclrc`` file.

```
$ ocicl setup
Add the following to your ${HOME}/.sbclrc file:

#-ocicl
(when (probe-file #P"/home/green/.local/share/ocicl/ocicl-runtime.lisp")
  (load #P"/home/green/.local/share/ocicl/ocicl-runtime.lisp"))
```

The default behavior for the runtime is to invoke ``ocicl`` when ASDF
tries to load a system that it can't find.

Try running this:
```
$ sbcl --eval "(asdf:load-system :str)"
```

Now look at your current directory.  You should see a directory called
``systems`` and a file called ``systems.csv``.  The ``systems``
directory contains the code you just downloaded, and ``systems.csv``
contains a mapping of system names to OCI artifacts and ``.asd``
files.

```
str.test, ghcr.io/ocicl/str@sha256:0903b59c33d3026ac55a6f4b25a79094d08e3110758d8ae728bf4188db659313, cl-str-20230511-b1c8380/str.test.asd
str, ghcr.io/ocicl/str@sha256:0903b59c33d3026ac55a6f4b25a79094d08e3110758d8ae728bf4188db659313, cl-str-20230511-b1c8380/str.asd
cl-ppcre, ghcr.io/ocicl/cl-ppcre@sha256:5274824d397fa197d5c7790344ace27f2a30fc34c6cadb0a9fcce7d1e4052486, cl-ppcre-20230511-b4056c5a/cl-ppcre.asd
cl-ppcre-unicode, ghcr.io/ocicl/cl-ppcre@sha256:5274824d397fa197d5c7790344ace27f2a30fc34c6cadb0a9fcce7d1e4052486, cl-ppcre-20230511-b4056c5a/cl-ppcre-unicode.asd
cl-unicode, ghcr.io/ocicl/cl-unicode@sha256:b61ac07aed06c926720e6a4c155fd0c9411b01a05ee7ebba55fca7df491880e5, cl-unicode-20230511-2790a6b/cl-unicode.asd
flexi-streams, ghcr.io/ocicl/flexi-streams@sha256:091df0cda6006b19aa206b022bb6d06fd9d5e5787b6152b9f0ae6846926ac5e0, flexi-streams-20230511-74a1027/flexi-streams.asd
flexi-streams-test, ghcr.io/ocicl/flexi-streams@sha256:091df0cda6006b19aa206b022bb6d06fd9d5e5787b6152b9f0ae6846926ac5e0, flexi-streams-20230511-74a1027/flexi-streams-test.asd
trivial-gray-streams, ghcr.io/ocicl/trivial-gray-streams@sha256:e82a60fdccc33916f26b60a3af63ee110f0b364cc2af59eee4be86256e8ea2b6, trivial-gray-streams-20230511-2b3823e/trivial-gray-streams.asd
trivial-gray-streams-test, ghcr.io/ocicl/trivial-gray-streams@sha256:e82a60fdccc33916f26b60a3af63ee110f0b364cc2af59eee4be86256e8ea2b6, trivial-gray-streams-20230511-2b3823e/trivial-gray-streams-test.asd
cl-change-case, ghcr.io/ocicl/cl-change-case@sha256:61791ee49f0160adad694eedbe8804fe9bcebad54336b0fbb8ce1a82091e20fa, cl-change-case-0.2.0/cl-change-case.asd
```

The next time you try to load ``str``, ASDF will load the code that
you've already downloaded and compiled.

Now try deleting the ``systems`` directory, and loading ``str`` again
as above.  ``ocicl`` will download the exact version specified in the
``systems.csv`` file.  The idea here is that you would commit your
``systems.csv`` file to your project's source repo, but never the
``systems`` directory.  When you run your program, you will always be
using the library versions locked in your ``systems.csv`` file.

Now let's try the ``ocicl`` command line tool.

```
$ ocicl help
ocicl 1.0.0 - copyright (C) 2023 Anthony Green <green@moxielogic.com>

Usage: ocicl [-h|--help] [-v|--verbose] command

Available options:
  -v, --verbose            produce verbose output

Choose from the following ocicl commands:

   help                                Print this help text
   install [SYSTEM[:VERSION]]...       Install systems
   latest [SYSTEM]...                  Install latest version of systems
   list SYSTEM...                      List available system versions
   setup                               Mandatory ocicl configuration
   version                             Show the ocicl version information

Distributed under the terms of the MIT License
```

If we again delete the ``systems`` directory, running ``ocicl
install`` will download all of the systems specified in your
``systems.csv`` file.

```
$ ocicl install
; downloading ghcr.io/ocicl/str@sha256:0903b59c33d3026ac55a6f4b25a79094d08e3110758d8ae728bf4188db659313
; downloading ghcr.io/ocicl/cl-ppcre@sha256:5274824d397fa197d5c7790344ace27f2a30fc34c6cadb0a9fcce7d1e4052486
; downloading ghcr.io/ocicl/cl-unicode@sha256:b61ac07aed06c926720e6a4c155fd0c9411b01a05ee7ebba55fca7df491880e5
; downloading ghcr.io/ocicl/flexi-streams@sha256:091df0cda6006b19aa206b022bb6d06fd9d5e5787b6152b9f0ae6846926ac5e0
; downloading ghcr.io/ocicl/trivial-gray-streams@sha256:e82a60fdccc33916f26b60a3af63ee110f0b364cc2af59eee4be86256e8ea2b6
; downloading ghcr.io/ocicl/cl-change-case@sha256:61791ee49f0160adad694eedbe8804fe9bcebad54336b0fbb8ce1a82091e20fa
```

You can download additional systems like so:
```
$ ocicl install trivial-garbage
; downloading trivial-garbage
; downloaded ghcr.io/ocicl/trivial-garbage@sha256:1eaadc3a546aaad7b452197663d9baece7a7e11beac6beb7db5b5faf4e74d541
; compiling file "/home/green/test/systems/trivial-garbage-20230511-b3af9c0/trivial-garbage.lisp" (written 19 MAY 2023 08:34:59 AM):

; wrote /home/green/.cache/common-lisp/sbcl-2.3.2-linux-x64/home/green/test/systems/trivial-garbage-20230511-b3af9c0/trivial-garbage-tmpMQBOWYP9.fasl
; compilation finished in 0:00:00.007
```

This downloads the latest version of trivial-garbage, which is the OCI
image with the ``latest`` tag, and is equivalent to ``ocicl install
trivial-garbage:latest``.

To see what other versions of a package are available, run ``ocicl list trivial-garbage``
```
$ ocicl list trivial-garbage
trivial-garbage:
 latest
 20230511-b3af9c0
```

Here we only have one version, 20230511-b3af9c0, which also has the
``latest`` tag.  Many lisp libraries are built from git sources
without release tags.  In this case, the version label represents the
build date and the git commit hash (b3af9c0).

To install any specific version of a system, just use the appropriate
version label in your ``ocicl install`` command.

To update all systems in your ``systems.csv`` file to the latest
version, run ``ocicl latest``.

You can change the default behaviour of downloading systems on demand
by setting ``ocicl-runtime:*download*`` to nil.

Security
--------

All system tarballs are digitally signed with the ocicl-tarball-signer
key: B96ACDBF35C5C1AB81596FB6D3AFE1884397BDC8.

You can download the unexpanded tarballs like so:
```
$ ocicl-oras pull ghcr.io/ocicl/str:latest
Downloading 577fc7118b8a cl-str-20230511-b1c8380.tar.gz
Downloaded  577fc7118b8a cl-str-20230511-b1c8380.tar.gz
Pulled [registry] ghcr.io/ocicl/str:latest
Digest: sha256:0903b59c33d3026ac55a6f4b25a79094d08e3110758d8ae728bf4188db659313

$ ls -l
total 32
-rw-r--r--. 1 green green 24609 May 19 09:02 cl-str-20230511-b1c8380.tar.gz
```

Similarly, the signature is available by appending ``.sig`` to the system name.
```
$ ocicl-oras pull ghcr.io/ocicl/str.sig:latest
Downloading 2a97da913ef7 cl-str-20230511-b1c8380.tar.gz.sig
Downloaded  2a97da913ef7 cl-str-20230511-b1c8380.tar.gz.sig
Pulled [registry] ghcr.io/ocicl/str.sig:latest
Digest: sha256:47903679d96504c5e83f08f7d6dfc4e613e7ab968e44dc46cb13b29f7917ddea
```

You can verify the signature like so:
```
$ gpg --verify cl-str-20230511-b1c8380.tar.gz.sig cl-str-20230511-b1c8380.tar.gz
gpg: Signature made Thu 11 May 2023 05:44:45 AM EDT
gpg:                using RSA key B96ACDBF35C5C1AB81596FB6D3AFE1884397BDC8
gpg: Good signature from "ocicl-tarball-signer" [ultimate]
```

These signatures are also archived in the
[sigstore](https://www.sigstore.dev) [rekor transparency
log](https://docs.sigstore.dev/rekor/overview/).  This gives you and
your auditors confidence that the code you are running is what it
claims to be.

You can search for these signatures based on the sha of the tarball
like so:
```
$ rekor-cli search --sha $(sha256sum cl-str-20230511-b1c8380.tar.gz)
Found matching entries (listed by UUID):
24296fb24b8ad77a6594635675d0e6365b89ee0d5e3b1ce823adb19c28aa3602c2537163710638d9

$ rekor-cli get --uuid 24296fb24b8ad77a6594635675d0e6365b89ee0d5e3b1ce823adb19c28aa3602c2537163710638d9
LogID: c0d23d6ad406973f9559f3ba2d1ca01f84147d8ffc5b8445c224f98b9591801d
Index: 20300488
IntegratedTime: 2023-05-11T09:44:49Z
UUID: 24296fb24b8ad77a6594635675d0e6365b89ee0d5e3b1ce823adb19c28aa3602c2537163710638d9
Body: {
  "RekordObj": {
    "data": {
      "hash": {
        "algorithm": "sha256",
        "value": "577fc7118b8a21285ad871dd44e4fe25126fd05d2d4fad52a4015d5a01788d44"
      }
    },
    "signature": {
      "content": "LS0tLS1CRUdJTiBQR1AgU0lHTkFUVVJFLS0tLS0KCmlRR3pCQUFCQ2dBZEZpRUV1V3JOdnpYRndhdUJXVysyMDYvaGlFT1h2Y2dGQW1SY3VRMEFDZ2tRMDYvaGlFT1gKdmNqcGh3d0FsSUJ6N3IrcnhZSml5dHhHZlFXMDJ6Z0tzQ1BKcC9RRXI1NUdIZjBQN3U0QlBod0ZmRlFRbWhRWQpsYndoclpjMEcvRFRXdm5vdzBOa0RTRXFBbVhtUjIyMzJOWDFEMVVBSEVRYWUzc3lhbld0aTd4ZEhLdXI3TE90CmsyRmFFMFl0VGt0a1RscDBzSGlxazliWHkzVVpqUHBFazBWZzZCaTM2QVUzVVFCMHFpc1dKQ2o4RGVLZnhSN1EKdkgvblo0MnJSMUNsTkRhdzBXQWc5eFR0WmNCSTRydEM4UXFIbWIzQ2N6elJ2WVM3T3V2VFRaM1h4NkNPQVFjUgpjVHNKa25qSGI4MXFQNFlBNDFiQ3l1L28xWGVCUmxIM1ZXVURyWHBoWEhETm1FcFFaTVFpWVBOYUl2Q1dOQ09lCkRqSFhLazk3NnFBcFVzVHBxcFRIdmgxUGNxSFpFeFdPRWQwSkxpR3BzZW9vODN4M1k0bTBDaXRBTDhDK2xzTk4KTGxWeFNCbmZ6STJVZnpBK3lWMFVVT1pHMXhJY1QzMVNaRGRZV1VKdG9OZmVuSnA0RTNXdlZzZXA3UDhXMXZxOApHY0RVU2lxT010ajFIdHhqTmdMTldCUk01aDNaMHhyaHQ0SjFRejArZHVESGxCOWJxeU9OS240eGtLZnBrWlRBCkRUakJkZTJlCj1DdS9LCi0tLS0tRU5EIFBHUCBTSUdOQVRVUkUtLS0tLQo=",
      "format": "pgp",
      "publicKey": {
        "content": "LS0tLS1CRUdJTiBQR1AgUFVCTElDIEtFWSBCTE9DSy0tLS0tCgp4c0ROQkdSYnIrY0JEQUM5YUlXaElpc08zNjRCdnhYc093QjZwUklKWGQrbWJINHp1VXdRNHIrcmQvb3M0TG1SCkxaaVRDekVZbVMrcHhnUWQwb1haR2o5elZ3VzRYMndaVzI4K0RVRDljWnpHL1J6RkRHeExnZzc1d2dDYmp1czcKeVc0UmtQQkhYOXRsNUFuRHU0SjFKK01Ra3hyUEVHZFpFSVp2QVlKOVJFWDRDSVhHQUNjdXgyaGtaVUlTNVpxZQpvdjRwd1FReFhTNm5SOVpUOHpieWo1SkhQWmtRek02eU42c0F6aXFlUUZpK3pKdXJaTG9kdU9BbEs2Z3BjY1F1CkRnQ0JCM280S2cvSk5lZVB0VTlHYUxHSXB1ZGdxd0N2VHpSbUR0NUc5bjd0cVJaV0ZFMitxSmg5MVBFdzhyUjkKOG5NQWxZSFI1S3BJcUpwbldjOFRSME0yQ0tCOEd2UzNjS3E3NHYvQTZ5bUFPNzErbmVad3pxTENyTEsydllNeApHbjdOSUVKM0grRVFQcEplNGkrbWxoeGE5Rm1zMi9qSUdjemJMNGtSbGdqVE9wazFmOE4zaWZDbnp1YkpxQ3ZXCnZGbWMxM2I0eE9ic3BlWndjckk3Kzg2R092a2FaZk1ONWZweGt4U1lETStLSVEyZlNYcXp0ZGlHWDBVZEp3dG4KOEZhTlBnTUJvckFGMzAwQUVRRUFBYzBVYjJOcFkyd3RkR0Z5WW1Gc2JDMXphV2R1WlhMQ3dSRUVFd0VJQURzVwpJUVM1YXMyL05jWEJxNEZaYjdiVHIrR0lRNWU5eUFVQ1pGdXY1d0liQXdVTENRZ0hBZ0lpQWdZVkNna0lDd0lFCkZnSURBUUllQndJWGdBQUtDUkRUcitHSVE1ZTl5TEZ6Qy85TmJmVUIzYncyMUhHQStBTk03WHg5VVdpZVlNSzkKcWlFUUFyYVgvQm85cnl6c3ZtcnUxcDZYN2RsTmtoTEJzMWl1MFZ0eFB4SWhHdUdncHNPekV2N0Y3OFdvM0pwNQo4dVQ5cW5kelV2aXh2S2lFTTJsdzZHZWFrallCMjV4Zy9VSnpWNmxGYWhhcVZ6emhUSkovbkI5YU9jNDg1WjBpCnV1UnIxU3Nkb3VHbHdRTVZydktRbU1rMjlaTzlKcnNIUmxQR2hnY1p0K2J1bUJPdDNKZTQ0WGtkL0NtNDBxdXYKb2Mwb0FFNGN2c2JkSmE2d3EwODlQK3VPWFZNZlhpUDVBd3ZWSjYxQ2NEYWRJWk1kT29KYnc0b1k3V2dWS2NIWQpYd0tlS0NzOFpzRWs2OGlTRm1FQ3Q3THI4U2tENVcwRXY0QWV3clh4dEFXNElCYUtRaE1pSEZsQ2kvdzdLdjduCm1lM0t4Q1JEc0F4NEdld05iTXRaUjBnZHRwV21CWGVpRytROC8wZGdMUFRqRmFBWHFRQithUWNuMzFscnZCYVgKd0Jib3FtUlNtWDVwdk1uaHhiNSt0R0hsRU9QekMrZ0k0VU50cVZ4L1NIRCtSZHNNM0owd2huRjh3R09KWlRZUwo1WkxrZWR6Qjc3VEtzakNhci92Q2x5UTUvalIrck81VjlwSE93TTBFWkZ1djV3RU1BTUJkY1JYYUpCQ2V2dHV1CklnU3Nmc3hDb3NLRnJQN3JTdmN2bHlHT3pCVlBKT2JFbE5NbjVTOGNwZCtuMXcrQS9rdXFobUNHVmJjaHI2YkMKUERRbmJad2pwWUU3bGVISGtQT2tkT2JHY3VOWjZ1YVNsSktWcWg5aHpHeHZZUlhIMFhUSDNkb2NJTTdyRkdtOQpSMWdKcDhBNmtSbWg4ZVNSOWpMRWNhS2lRTUZPbEYzU3RFZms4VzFsZ0x2Z2VTVnJkc0s0Rnc3Ui9BRVlmek5xClpTZEdRYmx0WXkyekVKcG04M2QxN1BCZXN4eE5FdVNWYUlibCtqai9TLzVkL1h0OGVKdVJzbDVOeDdLSTRpenkKK1pFWkxOWkJZa3FzMTZsSnRva2oxclZOUitLWEtCN1pDenVacGxjYURJbExWMUN0YXFqVXpMazFlcVh0TXBZOQordTFocXd0em5vWnRsTTZTTmVYZnUzS3YzSFlaWUlJVGcrSDBLQVVnZko1S1F1YnNaN2dmSVpaMno0WkZHUHJPCllhWlRJQmNGMGlIbEhZWitPTVRONWpKR2VrU0NlTW0yVUZzZVBqSUtQdHhudTk0ajNDanhWbmlzZURzUkJja1oKcFRaY2VPNkZVa3pCYkltQ2FqNklZd2NmdFhBWVRFQjZXdzE0OUdFYnpONEVyV1pKaXdBUkFRQUJ3c0QyQkJnQgpDQUFnRmlFRXVXck52elhGd2F1QldXKzIwNi9oaUVPWHZjZ0ZBbVJicitjQ0d3d0FDZ2tRMDYvaGlFT1h2Y2lGCmxndi9URndrYXYzYWFFVnhvU0hDNDJzTGl0YmZGOC83YW53ZGZPZkhFdFVSejBmdk1vVEVsamdtS05jb3FQYkEKdkhJMmN1MXA3RTAvOXNZY0VTaXlPNzJVR29oWmZtaWFsTlhROE53TVcySzFnM1FZS0hKam55WWV2WnlkV3dlegpDbXovU0RpNXFBYmMzSVprTFZXRk5LOXdrenljVlhYcVB6ZnJzdDkyTXJ1ZktYSDc2eEExZlA2NXl6S21ZSW9WCnc3eHJkaGp2VzMvS0JHeU5iZHk0dmNucWdERUFCbWR5OUJxRkhKK0p4QnFIZXZKTjV6SXAySHFRK2x4YWVGYmgKQTllckJKYVMzNDU4eXVxd0FvTEJ4OURscHZYQzE2c3NXVVU3WGlJQ1pFb255aXVxQVN4QXMxZjk3SFNjVWx3UQpxWExLUURmZEFud25PeE9wcXFHS1E1M3FzN3UxdUJ0RzJrN0JQMmEwbE1EUW9Za1hoK2tPNTRVT0ZtS1lFNGdvCjE0SFdYSmc3S0Y5UjMxQ0ZrNXNGUU9yYmR1bCtoNC80VVRMSFhGTFBMVkkvTzYwZDNCNkd4cm91SlRSeWJDUHAKV0tvZjg0VGcyY1FiV2FmVko5bzlIbklwb2lPNGJCMVZrQndoM3E2TTE2L0kyWC9zNGhaVVNLODJYSWJ0TlFxRgpRZmxZCj1hbnlKCi0tLS0tRU5EIFBHUCBQVUJMSUMgS0VZIEJMT0NLLS0tLS0="
      }
    }
  }
}
```

Further explanation of the sigstore tooling and ecosystem is beyond
the scope of this document, but you can read about it at
[https://docs.sigstore.dev/](https://docs.sigstore.dev/).

Self-Hosting
------------

[This section needs to be written on hosting the OCI content in your
own registry -- something very easy to do, as there are many useful
tools to borrow from the container world.]

Systems
-------

Systems managed by ``ocicl`` are maintained in github, at
``https://github.com/ocicl``.  Each system has its own source repo,
and the ``README.org`` file contains everything required to build and
publish to the OCI registry via github actions.  Contributions are
welcome and appreciated!

Tips and Troubleshooting
------------------------

You may find it convenient to tell ASDF to load from the current directory.
Do this by placing the following in your ``.sbclrc`` file:
```
(pushnew (uiop:getcwd) asdf:*central-registry*)
```

Setting ``ocicl-runtime:*verbose*`` to ``t`` will output useful and
interesting log info.

Author and License
-------------------

``ocicl`` was written by [Anthony Green](https://github.com/atgreen),
and is distributed under the terms of the MIT license.

This software includes Lisp source code files written by Zachary
Beane, Mark Karpov, and PMSF IT Consulting Pierre R. Mai.  See the
ocicl source files for details.
