(in-package #:org.shirakumo.legit)

;; low-level.lisp
(docs:define-docs
  (function git-add
    "git add

See https://git-scm.com/docs/git-add")

  (function git-am
    "git am

See https://git-scm.com/docs/git-am")

  (function git-apply
    "git apply

See https://git-scm.com/docs/git-apply")

  (function git-archive
    "git archive

See https://git-scm.com/docs/git-archive")

  (function git-bisect
    "git bisect

See https://git-scm.com/docs/git-bisect")

  (function git-blame
    "git blame

See https://git-scm.com/docs/git-blame")

  (function git-branch
    "git branch

See https://git-scm.com/docs/git-branch")

  (function git-bundle
    "git bundle

See https://git-scm.com/docs/git-bundle")

  (function git-cat-file
    "git cat-file

See https://git-scm.com/docs/git-cat-file")

  (function git-checkout
    "git checkout

See https://git-scm.com/docs/git-checkout")

  (function git-cherry-pick
    "git cherry-pick

See https://git-scm.com/docs/git-cherry-pick")

  (function git-clean
    "git clean

See https://git-scm.com/docs/git-clean")

  (function git-clone
    "git clone

See https://git-scm.com/docs/git-clone")

  (function git-commit
    "git commit

See https://git-scm.com/docs/git-commit")

  (function git-commit-tree
    "git commit-tree

See https://git-scm.com/docs/git-commit-tree")

  (function git-config
    "git config

See https://git-scm.com/docs/git-config")

  (function git-count-objects
    "git count-objects

See https://git-scm.com/docs/git-count-objects")

  (function git-daemon
    "git daemon

See https://git-scm.com/docs/git-daemon")

  (function git-describe
    "git describe

See https://git-scm.com/docs/git-describe")

  (function git-diff
    "git diff

See https://git-scm.com/docs/git-diff")

  (function git-diff-index
    "git diff-index

See https://git-scm.com/docs/git-diff-index")

  (function git-fast-import
    "git fast-import

See https://git-scm.com/docs/git-fast-import")

  (function git-fetch
    "git fetch

See https://git-scm.com/docs/git-fetch")

  (function git-filter-branch
    "git filter-branch

See https://git-scm.com/docs/git-filter-branch")

  (function git-for-each-ref
    "git for-each-ref

See https://git-scm.com/docs/git-for-each-ref")

  (function git-format-patch
    "git format-patch

See https://git-scm.com/docs/git-format-patch")

  (function git-fsck
    "git fsck

See https://git-scm.com/docs/git-fsck")

  (function git-gc
    "git gc

See https://git-scm.com/docs/git-gc")

  (function git-grep
    "git grep

See https://git-scm.com/docs/git-grep")

  (function git-hash-object
    "git hash-object

See https://git-scm.com/docs/git-hash-object")

  (function git-help
    "git help

See https://git-scm.com/docs/git-help")

  (function git-init
    "git init

See https://git-scm.com/docs/git-init")

  (function git-instaweb
    "git instaweb

See https://git-scm.com/docs/git-instaweb")

  (function git-log
    "git log

See https://git-scm.com/docs/git-log")

  (function git-ls-files
    "git ls-files

See https://git-scm.com/docs/git-ls-files")

  (function git-merge
    "git merge

See https://git-scm.com/docs/git-merge")

  (function git-merge-base
    "git merge-base

See https://git-scm.com/docs/git-merge-base")

  (function git-mergetool
    "git mergetool

See https://git-scm.com/docs/git-mergetool")

  (function git-mv
    "git mv

See https://git-scm.com/docs/git-mv")

  (function git-pull
    "git pull

See https://git-scm.com/docs/git-pull")

  (function git-push
    "git push

See https://git-scm.com/docs/git-push")

  (function git-read-tree
    "git read-tree

See https://git-scm.com/docs/git-read-tree")

  (function git-rebase
    "git rebase

See https://git-scm.com/docs/git-rebase")

  (function git-reflog
    "git reflog

See https://git-scm.com/docs/git-reflog")

  (function git-remote
    "git remote

See https://git-scm.com/docs/git-remote")

  (function git-request-pull
    "git request-pull

See https://git-scm.com/docs/git-request-pull")

  (function git-reset
    "git reset

See https://git-scm.com/docs/git-reset")

  (function git-rev-list
    "git rev-list

See https://git-scm.com/docs/git-rev-list")

  (function git-rev-parse
    "git rev-parse

See https://git-scm.com/docs/git-rev-parse")

  (function git-revert
    "git revert

See https://git-scm.com/docs/git-revert")

  (function git-rm
    "git rm

See https://git-scm.com/docs/git-rm")

  (function git-send-email
    "git send-email

See https://git-scm.com/docs/git-send-email")

  (function git-shortlog
    "git shortlog

See https://git-scm.com/docs/git-shortlog")

  (function git-show
    "git show

See https://git-scm.com/docs/git-show")

  (function git-show-ref
    "git show-ref

See https://git-scm.com/docs/git-show-ref")

  (function git-stash
    "git stash

See https://git-scm.com/docs/git-stash")

  (function git-status
    "git status

See https://git-scm.com/docs/git-status")

  (function git-submodule
    "git submodule

See https://git-scm.com/docs/git-submodule")

  (function git-svn
    "git svn

See https://git-scm.com/docs/git-svn")

  (function git-symbolic-ref
    "git symbolic-ref

See https://git-scm.com/docs/git-symbolic-ref")

  (function git-tag
    "git tag

See https://git-scm.com/docs/git-tag")

  (function git-update-index
    "git update-index

See https://git-scm.com/docs/git-update-index")

  (function git-update-ref
    "git update-ref

See https://git-scm.com/docs/git-update-ref")

  (function git-update-server-info
    "git update-server-info

See https://git-scm.com/docs/git-update-server-info")

  (function git-verify-pack
    "git verify-pack

See https://git-scm.com/docs/git-verify-pack")

  (function git-write-tree
    "git write-tree

See https://git-scm.com/docs/git-write-tree"))

;; process.lisp
(docs:define-docs
  (variable *git-output*
    "The stream to which the standard output of the git process is sent.

Defaults to T, but may be automatically bound by some functions in order to process the output.")

  (variable *git-errors*
    "The stream to which the error output of the git process is sent.

Defaults to T, but may be automatically bound by some functions in order to process the output.")

  (variable *git-input*
    "The stream from which the standard input of the git process is taken.

Defaults to NIL.")

  (function run-git
    "Launches a new git process with the given CMDARGS.

See LEGIT:RUN
See *GIT-OUTPUT*
See *GIT-ERRORS*
See *GIT-INPUT*"))

;; repository.lisp
(docs:define-docs
  (type repository
    "A class to represent a git repository with.

This is a LOCATION.

See LOCATION
See INIT")

  (function clear-cache
    "Clears the internal cache on the git repository.

Calling this causes subsequent queries against the repository to actually launch a subprocess. 
This should be called whenever the repository on the file system may have been modified in any way.

See REPOSITORY")

  (function git-location-p
    "Returns T if the given location/path is a, or within a git repository.")

  (function init
    "Initialises the given repository.

REPOSITORY can be a pathname or repository. Optionally supplied keyword arguments are
by default 

IF-DOES-NOT-EXIST  ::= :error          --- Signal an error if it does not exist.
                     | :ignore         --- Just return a repository instance.
                     | :create | :init --- Initialise a new, empty repository.
                     | :clone          --- Clone a new repository from a remote.
REMOTE             --- An optional remote path used to clone the repository, if
                       IF-DOES-NOT-EXIST is :CLONE
BRANCH             --- The branch to use if IF-DOES-NOT-EXIST is :CLONE, :CREATE, or
                       :INIT.
BARE               --- Whether the repository should be bare if IF-DOES-NOT-EXIST is
                       :CLONE, :CREATE, or :INIT.

See REPOSITORY
See GIT-INIT
See GIT-CHECKOUT
See GIT-CLONE")

  (function clone
    "Clone a repository from FROM to TO.

Default extra keyword arguments are BRANCH and BARE.

See GIT-CLONE")

  (function fetch
    "Fetch the latest commits from upstream.

This clears the cache.

See GIT-FETCH
See REPOSITORY
See CLEAR-CACHE")

  (function pull
    "Pull and merge the latest commits from upstream.

This clears the cache.

See GIT-PULL
See REPOSITORY
See CLEAR-CACHE")

  (function checkout
    "Checks out a specific THING in the repository.

This clears the cache.

See GIT-CHECKOUT
See REPOSITORY
See CLEAR-CACHE")

  (function reset
    "Resets changes in the repository.

Default extra keyword arguments are TO, HARD, MIXED, SOFT.

This clears the cache.

See GIT-RESET
See REPOSITORY
See CLEAR-CACHE")

  (function clean
    "Cleans the repository out.

Default extra keyword arguments are DIRECTORIES, FORCE, IGNORED.

See GIT-CLEAN
See REPOSITORY")

  (function git-value
    "Shorthand macro to handle caching of the value from the git process under NAME.")

  (function commits
    "Returns the full list of commit hashes on the repository.

See GIT-REV-LIST
See REPOSITORY")

  (function submodules
    "Returns a list of REPOSITORY instances that represent the submodules of REPOSITORY.

See GIT-SUBMODULE
See REPOSITORY")

  (function map-submodules
    "Maps FUNCTION over all submodules of REPOSITORY.

See SUBDMODULES
See REPOSITORY")

  (function do-submodules
    "Iterates over all submodules of REPOSITORY.

See MAP-SUBMODULES
See REPOSITORY")

  (function remotes
    "Returns an alist of all remotes on the REPOSITORY. The CAR is the name, the CDR the address.

This function is SETF-able, meaning you can PUSH new remotes to it or remove
remotes by SETF&REMOVE. It will also gracefully change the address of a remote
if it is changed in the setted list.

Setting REMOTES clears the cache.

See GIT-REMOTE
See REPOSITORY
See CLEAR-CACHE")

  (function commit-age
    "Returns a universal-time timestamp of when the given commit was made.

See GIT-LOG
See REPOSITORY")

  (function current-commit
    "Returns the latest commit hash on the current branch of the REPOSITORY.

See GIT-REV-PARSE
See REPOSITORY")

  (function current-branch
    "Returns the currently checked out branch of the REPOSITORY.

See GIT-REV-PARSE
See REPOSITORY")

  (function current-message
    "Returns the latest commit's message on the current branch of the REPOSITORY.

See GIT-LOG
See REPOSITORY")

  (function current-age
    "Returns the latest commit's age on the current branch of the REPOSITORY as a universal-time timestamp.

See GIT-LOG
See REPOSITORY")

  (function remote-url
    "Returns the address of a remote (default \"origin\") on the REPOSITORY.

Accepts a default keyword argument REMOTE.

See GIT-CONFIG
See REPOSITORY")

  (function default-remote
    "Returns a suitable default remote name.

Will get the branch's configured remote, if any, or fall back to the
first available remote.

See REPOSITORY")

  (function bare-p
    "Returns T if the REPOSITORY is bare.

See GIT-REV-PARSE
See REPOSITORY")

  (function branches
    "Returns a list of branch names for REPOSITORY.

See GIT-BRANCH
See REPOSITORY")

  (function tags
    "Returns a list of tag names for REPOSITORY.

See GIT-TAG
See REPOSITORY"))

;; toolkit.lisp
(docs:define-docs
  (function relative-dir
    "Get a directory pathname that is SUBDIRS beneath RELATIVE."))
