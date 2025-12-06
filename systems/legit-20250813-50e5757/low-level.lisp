(in-package #:org.shirakumo.legit)

(define-git-wrapper git-add
  &optional (paths :--)
  &key dry-run verbose force interactive patch edit update (all :bool)
  (ignore-removal :bool) intent-to-add refresh ignore-errors ignore-missing)

(define-git-wrapper git-am
  &optional maildirs
  &key signoff keep keep-non-patch (keep-cr :bool) (scissors :bool)
  (message-id :bool) quiet (utf8 :bool) 3way ignore-space-change ignore-whitespace
  (whitespace :arg=) (directory :arg=) (exclude :arg=) (include :arg=) reject
  patch-format interactive commiter-date-is-author-date ignore-date skip
  (gpg-sign :arg= :flag) continue resolved (resolvemsg :arg=) abort
  (remove-leading-slashes (:name p) :arg.) (surrounding (:name C) :upcase :arg.))

(define-git-wrapper git-apply
  &optional patches
  &key stat numstat summary check index cached 3way (build-fake-ancestor :arg=)
  recursive reject (machine-readable (:name z) :flag)
  (remove-leading-slashes (:name p) :arg.) (surrounding (:name C) :upcase :arg.)
  unidiff-zero apply no-add binary (exclude :arg=) (include :arg=) ignore-whitespace 
  (whitespace (:member :nowarn :warn :fix :error :error-all)) inaccurate-eof
  verbose recount (directory :arg=) unsafe-paths)

(define-git-wrapper git-archive
  tree-ish
  &optional paths
  &key (format :arg=) list verbose (prefix :arg=) (output :arg=) worktree-attributes
  (remote :arg=) (exec :arg=))

(define-git-wrapper git-bisect
  (action (:member :help :start :bad :good :skip :reset :visualize :replay :log :run))
  &optional options (paths :--)
  &key no-checkout)

(define-git-wrapper git-blame
  (file :--)
  &key (blank (:name b) :flag) root show-stats (range (:name L) :upcase (:map ","))
  (long (:name l) :flag) (timestamp (:name t) :flag) (revisions (:name S) :upcase :arg)
  reverse porcelain line-porcelain incremental (encoding :arg=) (contents :arg)
  (date :arg) (moved (:name M) :upcase :arg. :flag) (within-commit (:name C) :upcase :arg. :flag)
  (help (:name h) :flag) (annotate (:name c) :flag) score-debug show-name show-number
  (suppress (:name s) :flag) show-email (ignore-whitespace (:name w) :flag) (abbrev :arg=))

(define-git-wrapper git-branch
  &optional branch start-point old-branch new-branch patterns branches
  &key delete create-reflog force move (color :arg= :bool) (column :arg :bool)
  remotes all list verbose quiet (abbrev :arg= :bool) (track :bool) set-upstream
  (set-upstream-to :arg=) unset-upstream edit-description (contains :arg)
  (merged :arg) (no-merged :arg) (format :arg=))

(define-git-wrapper git-bundle
  (action (:member :create :verify :list-heads :unbundle)) file
  &optional git-rev-list-args refnames)

(define-git-wrapper git-cat-file
  &optional type  object
  &key (show-type (:name t) :flag) (show-size (:name s) :flag) (suppress (:name e) :flag)
  (pretty (:name p) :flag) textconv (batch :flag :arg=) (batch-check :flag :arg=)
  allow-unknown-type follow-symlinks)

(define-git-wrapper git-checkout
  &optional branch new-branch start-point tree-ish commit (paths :--)
  &key quiet force ours theirs (track :bool) detach (orphan :arg)
  ignore-skip-worktree-bits merge (cnoflict :arg=) path
  ignore-other-worktrees)

(define-git-wrapper git-cherry-pick
  &optional commits
  &key edit continue quit abort (append-notice (:name x) :flag) (mainline :arg)
  no-commit signoff (gpg-sign :flag :arg=) ff allow-empty allow-empty-message
  keep-redundant-commits (strategy :arg=) (strategy-option :arg=))

(define-git-wrapper git-clean
  &optional (paths :--)
  &key (directories (:name d) :flag) force interactive dry-run quiet (exclude :arg=)
  (no-ignore (:name x) :flag) (remove-ignored (:name X) :upcase :flag))

(define-git-wrapper git-clone
  repository
  &optional directory
  &key local no-hardlinks shared (reference :arg) dissociate quiet verbose progress
  no-checkout bare mirror (origin :arg) (branch :arg) (upload-pack :arg)
  (template :arg=) (config :map) (depth :arg) (single-branch :bool) recursive
  (separate-git-dir :arg=))

(define-git-wrapper git-commit
  &optional (files :--)
  &key all patch (reuse-message :arg=) (reedit-message :arg=) (fixup :arg=)
  (squash :arg=) reset-author short branch porcelain long null (file :arg=)
  (author :arg=) (date :arg=) (message :arg=) (template :arg=) signoff no-verify
  allow-empty allow-empty-message
  (cleanup (:member :strip :whitespace :verbatim :scissors :default)) (edit :bool)
  amend no-post-rewrite include only (untracked-files :flag :arg=) verbose quiet
  dry-run (status :bool) (gpg-sign :bool :arg=))

(define-git-wrapper git-commit-tree
  tree
  &key (parent (:name p) :arg) (message (:name m) :arg) (file (:name F) :upcase :arg)
  (gpg-sign :bool :arg=))

(define-git-wrapper git-config
  &optional (file-option :front :opt) (type :front :opt) name value URL name-regex
  value-regex old-name new-name default stdout-is-tty
  &key null add replace-all unset unset-all rename-section remove-section list
  get-color get-colorbool edit)

(define-git-wrapper git-count-objects
  &key verbose human-readable)

(define-git-wrapper git-daemon
  &optional directory
  &key strict-paths (base-path :arg=) base-path-relaxed (interpolated-path :arg=)
  export-all inted (listen :arg=) (port :arg=) (init-timeout :arg=) (timeout :arg=)
  (max-connections :arg=) syslog (user-path :flag :arg=) verbose reuseaddr detach
  (pid-file :arg=) (user :arg=) (group :arg=) (enable :arg=) (disable :arg=)
  (allow-override :arg=) (forbid-overrif :arg=) (informative-errors :bool)
  (access-hook :arg=))

(define-git-wrapper git-describe
  &optional commit-ishs
  &key (dirty :flag :arg=) all tags contains (abbrev :arg=) (candidates :arg=)
  exact-match debug long (match :arg) always first-parent)

(define-git-wrapper git-diff
  &optional commit blob (paths :--)
  &key (patch :bool) (unified :arg=) raw patch-with-raw minimal patience histogram
  (diff-algorithm (:member :patience :minimal :histogram :myers)) (stat :arg=) numstat
  shortstat (dirstat :arg=) summary patch-with-stat (null (:name z) :flag) name-only
  name-status (submodule :arg=) (color :bool :arg=) (word-diff :arg=)
  (word-diff-regex :arg=) (color-words :arg=) no-renames check
  (ws-error-highlight :arg=) full-index binary (abbrev :arg=) (break-rewrites :arg=)
  (find-renames :arg=) (find-copies :arg=) find-copies-harder irreversible-delete
  (limit-find (:name l) :arg.) (diff-filter :arg=)
  (differing-occurrences (:name S) :upcase :arg.)
  (differing-diffs (:name G) :upcase :arg.)
  pickaxe-all pickaxe-regex (order (:name O) :upcase :arg.)
  (swap (:name R) :upcase :flag) (relative :arg=) text ignore-space-at-eol
  ignore-space-change ignore-all-space ignore-blank-lines (inter-hunk-context :arg=)
  function-context (ext-diff :bool) (textconv :bool) (ignore-submodules :flag :arg=)
  (src-prefix :arg=) (dst-prefix :arg=) no-prefix)

(define-git-wrapper git-diff-index
  tree-ish
  &optional paths
  &key (patch :bool) (unified :arg=) raw patch-with-raw minimal patience histogram
  (diff-algorithm (:member :patience :minimal :histogram :myers)) (stat :arg=) numstat
  shortstat (dirstat :arg=) summary patch-with-stat (null (:name z) :flag) name-only
  name-status (submodule :arg=) (color :bool :arg=) (word-diff :arg=)
  (word-diff-regex :arg=) (color-words :arg=) no-renames check
  (ws-error-highlight :arg=) full-index binary (abbrev :arg=) (break-rewrites :arg=)
  (find-renames :arg=) (find-copies :arg=) find-copies-harder irreversible-delete
  (limit-find (:name l) :arg.) (diff-filter :arg=)
  (differing-occurrences (:name S) :upcase :arg.)
  (differing-diffs (:name G) :upcase :arg.)
  pickaxe-all pickaxe-regex (order (:name O) :upcase :arg.)
  (swap (:name R) :upcase :flag) (relative :arg=) text ignore-space-at-eol
  ignore-space-change ignore-all-space ignore-blank-lines (inter-hunk-context :arg=)
  function-context (ext-diff :bool) (textconv :bool) (ignore-submodules :flag :arg=)
  (src-prefix :arg=) (dst-prefix :arg=) no-prefix (report-unchecked (:name m) :flag)
  cached)

(define-git-wrapper git-fast-import
  &key force quiet stats (cat-blob-fd :arg=) (date-format :arg=) done
  (export-marks :arg=) (import-marks :arg=) (import-marks-if-exists :arg=)
  (relative-marks :bool) (active-branches :arg=) (big-file-threshold :arg=)
  (depth :arg=) (export-pack edges :arg=) (max-pack-size :arg=))

(define-git-wrapper git-fetch
  &optional repository refspecs group repositories groups
  &key all append (depth :arg=) unshallow update-shallow dry-run force
  keep multiple prune no-tags (refmap :arg=) tags
  (recurse-submodules (:member :yes :on-demand :no) :bool) (submodule-prefix :arg=)
  (recurse-submodules-default (:member :yes :on-demand)) update-head-ok
  (upload-pack :arg) quiet verbose progress)

(define-git-wrapper git-filter-branch
  &optional (options :--)
  &key (env-filter :arg) (tree-filter :arg) (index-filter :arg) (parent-filter :arg)
  (msg-filter :arg) (commit-filter :arg) (tag-name-filter :arg)
  (subdirectory-filter :arg) prune-empty (original :arg) (directory (:name d) :arg)
  force)

(define-git-wrapper git-for-each-ref
  &optional patterns
  &key (count :arg=) shell perl python tcl (sort :arg=) (format :arg=))

(define-git-wrapper git-format-patch
  &optional options since revision-range
  &key no-stat no-patch (unified :arg=) minimal patience histogram
  (diff-algorigthm (:member :default :myers :minimal :patience :histogram))
  (stat :arg=) numstat shortstat (dirstat :arg=) summary no-renames full-index
  binary (abbrev :flag :arg=) (break-rewrites :flag :arg=) (find-renames :flag :arg=)
  (find-copies :flag :arg=) find-copies-harder irreversible-delete
  (limit-find (:name l) :arg.) (order (:name O) :upcase :arg.) text ignore-space-at-eol
  ignore-space-change ignore-all-space ignore-blank-lines (inter-hunk-context :arg=)
  function-context (ext-diff :bool) (textconv :bool) (ignore-submodules :flag :arg=)
  (src-prefix :arg=) (dst-prefix :arg=) no-prefix (output-directory :arg)
  (numbered :bool) (start-number :arg) numbered-files keep-subject signoff stdout
  (attach :arg= :bool) (inline :arg=) (thread :arg= :bool) (in-reply-to :arg=)
  ignore-if-in-upstream (subject-prefix :arg=) (reroll-count :arg=) (to :arg=)
  (cc :arg=) (from :arg= :flag) (add-header :arg=) (cover-letter :bool)
  (notes :flag :arg=) (signature :bool :arg=) (signature-file :arg=) (suffix :arg=)
  quiet no-binary root)

(define-git-wrapper git-fsck
  &optional objects
  &key unreacakble (dangling :bool) root tags cache no-reflogs full strict verbose
  lost-found (progress :bool))

(define-git-wrapper git-gc
  &key aggressive auto (prune :arg= :bool) quiet force)

(define-git-wrapper git-grep
  &optional tree (paths :--)
  &key cached no-index untracked (exclude-standard :bool) text (textconv :bool)
  ignore-case (no-binary (:name I) :upcase :flag) (max-depth :arg) word-regexp
  invert-match (suppress-filename (:name h) :flag) full-name extended-regexp
  basic-regexp perl-regexp fixed-strings line-number name-only null count
  (color :bool :arg=) break heading show-function (context :arg)
  (after-context :arg) (before-context :arg) function-context (file (:name f) :arg)
  (pattern (:name e) :arg) and or not all-match quiet)

(define-git-wrapper git-hash-object
  (files :--)
  &key (type (:name t) :arg) (write (:name w) :flag) stdin stdin-paths
  (path :arg=) no-filters literally)

(define-git-wrapper git-help
  command-or-guide
  &key all guides info man web)

(define-git-wrapper git-init
  &optional directory
  &key quiet bare (template :arg=) (separate-git-dir :arg=)
  (shared :arg= (:member :false :true :umask :group :all :world :everybody)))

(define-git-wrapper git-instaweb
  &key local (httpd :arg=) (module-path :arg=) (port :arg=) (browser :arg=)
  start stop restart)

(define-git-wrapper git-log
  &optional revision-range (paths :--)
  &key follow (decorate (:member :short :full :no) :bool) source use-mailmap
  full-diff log-size (range (:name L) :upcase (:map ",")) (max-count :arg=)
  (skip :arg=) (since :arg=) (until :arg=) (author :arg=) (grep-reflog :arg=)
  all-match invert-grep regexp-ignore-case basic-regexp extended-regexp
  fixed-strings perl-regexp remove-empty (merges :bool) (min-parents :arg= :bool)
  (max-parents :arg= :bool) first-parent not all (branches :arg=) (tags :arg=)
  (remotes :arg=) (glob :arg=) (exclude :arg=) reflog ignore-missing bisect
  stdin cherry-mark cherry-pick left-only right-only cherry walk-reflogs merge
  boundary simplify-by-decoration full-history dense sparse simplify-merges
  ancestry-path date-order author-date-order topo-order reverse
  (no-walk :flag (:member :sorted :unsorted)) do-walk relative-date
  (date (:member :relative :local :default :iso :iso-strict :rfc :short :raw))
  parents children left-right graph (show-linear-break :flag :arg=)
  (simultaneous-diff (:name c) :flag) (compressed-simultaneous-diff (:name c) :flag)
  (full-merge-diff (:name m) :flag) (show-recursive-diff (:name r) :flag)
  (show-tree-diff (:name t) :flag) (pretty :arg=) (format :arg=))

(define-git-wrapper git-ls-files
  &optional (files :--)
  &key cached deleted modified others ignored stage directory no-empty-directory
  unmerged killed (zero-terminate (:name z) :flag) (exclude :arg=)
  (exclude-from :arg=) (exclude-per-directory :arg=) exclude-standard error-unmatch
  (with-tree :arg=) full-name (abbrev :arg=) debug)

(define-git-wrapper git-ls-tree
  &optional tree-ish (paths :--)
  &key name-only name-status object-only full-name full-tree
  (format :arg=)
  (abbrev :arg=))

(define-git-wrapper git-merge
  &optional head commits
  &key (commit :bool) (edit :bool) (ff :bool) ff-only (log :bool :arg=) (stat :bool)
  (squash :bool) (strategy :arg=) (strategy-option :arg=) (verify-signatures :bool)
  (summary :bool) quiet verbose (progress :bool) (gpg-sign :flag :arg=)
  (message (:name m) :arg) (rerere-autoipdate :bool) abort)

(define-git-wrapper git-merge-base
  &optional ref commits
  &key octopus independent is-ancestor fork-point all)

(define-git-wrapper git-mergetool
  &optional files
  &key (tool :arg=) tool-help (prompt :bool))

(define-git-wrapper git-mv
  sources destination
  &key fodce (skip-errors (:name k) :flag) dry-run verbose)

(define-git-wrapper git-pull
  &optional repository refspecs
  &key quiet verbose (recurse-submodules :bool (:member :yes :on-demand :no))
  (commit :bool) (edit :bool) (ff :bool) ff-only (log :bool :arg=) (stat :bool)
  (squash :bool) (strategy :arg=) (strategy-option :arg=) (verify-signatures :bool)
  (summary :bool) (rebase :bool (:member :false :true :perserve)) all append
  (depth :arg=) unshallow update-shallow force keep no-tags update-head-ok
  (upload-pack :arg) progress)

(define-git-wrapper git-push
  &optional repository refspecs
  &key all prune mirror dry-run porcelain delete tags follow-tags
  signed (atomic :bool) (receive-pack :arg=) (force-with-lease :bool :arg=) force
  repo set-upstream (thin :bool) quiet verbose progress
  (recurse-submodules (:member :check :on-demand)) (verify :bool))

(define-git-wrapper git-read-tree
  &optional tree-ish1 tree-ish2 tree-ish3
  &key (merge (:name m) :flag) reset (update (:name u) :flag)
  (temporary-index (:name i) :flag) dry-run (verbose (:name v) :flag) trivial
  aggressive (prefix :arg=) (exclude-per-directory :arg=) (index-output :arg=)
  no-sparse-checkout empty)

(define-git-wrapper git-rebase
  &optional upstream branch
  &key (onto :arg) continue abort keep-empty skip edito-todo merge (strategy :arg=)
  (strategy-option :arg=) (gpg-sign :arg= :flag) quiet verbose (stat :bool)
  (verify :bool) (context (:name C) :upcase :arg.) force-rebase (fork-point :bool)
  ignore-whitespace (whitespace (:member :nowarn :warn :fix :error :error-all))
  committer-date-is-author-date ignore-date interactive preserve-merges (exec :arg)
  root (autosquash :bool) (autostash :bool) no-ff)

(define-git-wrapper git-reflog
  (action (:member :show :expire :delete) :front)
  &key all (expire :arg=) (expire-unreachable :arg=) updateref rewrite stale-fix
  dry-run verbose follow (decorate (:member :short :full :no) :bool) source use-mailmap
  full-diff log-size (range (:name L) :upcase (:map ",")) (max-count :arg=)
  (skip :arg=) (since :arg=) (until :arg=) (author :arg=) (grep-reflog :arg=)
  all-match invert-grep regexp-ignore-case basic-regexp extended-regexp
  fixed-strings perl-regexp remove-empty (merges :bool) (min-parents :arg= :bool)
  (max-parents :arg= :bool) first-parent not (branches :arg=) (tags :arg=)
  (remotes :arg=) (glob :arg=) (exclude :arg=) reflog ignore-missing bisect
  stdin cherry-mark cherry-pick left-only right-only cherry walk-reflogs merge
  boundary simplify-by-decoration full-history dense sparse simplify-merges
  ancestry-path date-order author-date-order topo-order reverse
  (no-walk :flag (:member :sorted :unsorted)) do-walk relative-date
  (date (:member :relative :local :default :iso :iso-strict :rfc :short :raw))
  parents children left-right graph (show-linear-break :flag :arg=)
  (simultaneous-diff (:name c) :flag) (compressed-simultaneous-diff (:name c) :flag)
  (full-merge-diff (:name m) :flag) (show-recursive-diff (:name r) :flag)
  (show-tree-diff (:name t) :flag))

(define-git-wrapper git-remote
  (action (:member :add :rename :remove :set-head :set-branches :set-url :show :prune :update NIL) :front)
  &optional name url old new branches newurl oldurl names groups remotes
  &key (verbose :flag) (tags :bool) (mirror (:member :fetch :push)) auto add
  push delete dry-run prune (immediate (:name f) :flag) (track (:name t) :arg)
  (symlink (:name m) :arg) (no-query (:name n) :flag))

(define-git-wrapper git-request-pull
  start url
  &optional end
  &key (patch (:name p) :flag))

(define-git-wrapper git-reset
  &optional tree-ish (paths :--)
  &key soft mixed hard merge keep quiet patch)

(define-git-wrapper git-rev-list
  &optional commit (paths :--)
  &key (max-count :arg=) (skip :arg=) (since :arg=) (until :arg=) (max-age :arg=)
  (author :arg=) (grep-reflog :arg=) (grep :arg=) all-match invert-grep
  regexp-ignore-case basic-regexp extended-regexp fixed-strings perl-regexp
  remove-empty (merges :bool) (min-parents :arg= :bool) (max-parents :arg= :bool)
  first-parent all (branches :flag :arg=) (tags :flag :arg=) (remotes :flag :arg=)
  (glob :arg=) (exclude :arg=) reflog ignore-missing stdin quiet cherry-mark
  cherry-pick left-only right-only cherry walk-reflogs merge boundary use-bitmap-index
  simplify-by-decoration full-history dense sparse simplify-merges ancestry-path
  bisect bisect-vars bisect-all date-order author-date-order topo-order reverse
  objects objects-edge objects-edge-aggressive indexed-objects unpacked
  (no-walk (:member :sorted :unsorted) :flag) do-walk relative-date
  (date (:member :relative :local :default :iso :iso-strict :rfc :short :raw))
  header parents children timestamp left-right graph (show-linear-break :arg= :flag)
  count (full-merge-diff (:name m) :flag) (show-recursive-diff (:name r) :flag)
  (show-tree-diff (:name t) :flag))

(define-git-wrapper git-rev-parse
  revs
  &key parseopt sq-quote keep-dashdash stop-at-non-option stuck-long revs-only no-revs
  flags (default :arg) (prefix :arg) verify quiet (abbrev-ref :flag (:member :strict :loose))
  (short :flag :arg=) symbolic symbolic-full-name all (branches :arg=) (tags :arg=)
  (remotes :arg=) (glob :arg=) (exclude :arg=) (disambiguate :arg=) local-env-vars
  git-dir git-common-dir is-inside-git-dir is-inside-work-tree is-bare-repository
  (resolve-git-dir :arg) (git-path :arg) show-cdup show-prefix show-toplevel
  shared-index-path (since :arg=) (until :arg=))

(define-git-wrapper git-revert
  &optional commits
  &key (edit :bool) (mainline :arg) no-commit (gpg-sign :flag :arg=) signoff
  (strategy :arg=) (strategy-option :arg=) continue quit abort)

(define-git-wrapper git-rm
  (files :--)
  &key force dry-run (recursive (:name r) :flag) cached ignore-unmatch quiet)

(define-git-wrapper git-send-email
  things
  &key annotate (bcc :arg=) (cc :arg=) compose (from :arg=) (in-reply-to :arg=)
  (subject :arg=) (to :arg=) (8bit-encoding :arg=) (compose-encoding :arg=)
  (transfer-encoding (:member :7bit :8bit :quoted-printable :base64))
  (xmailer :bool) (envelope-sender :arg=) (smtp-encryption :arg=)
  (smtp-domain :arg=) (smtp-pass :flag :arg=) (smtp-server :arg=)
  (smtp-server-port :arg=) (smtp-server-option :arg=) smtp-ssl
  smtp-ssl-cert-path (smtp-user :arg=) (smtp-debug (:member :0 :1))
  (to-cmd :arg=) (cc-cmd :arg=) (chain-reply-to :bool) (identity :arg=)
  (signed-off-by-cc :bool) (cc-cover :bool) (to-cover :bool) (suppress-cc :arg=)
  (suppress-from :bool) (thread :bool)
  (confirm (:member :always :never :cc :compose :auto)) dry-run (format-patch :bool)
  quiet (validate :bool) force)

(define-git-wrapper git-shortlog
  &optional revision-range (paths :--)
  &key numbered summary email (format :arg=) (width (:name w) :arg.))

(define-git-wrapper git-show
  objects
  &key (pretty :flag :arg=) (abbrev-commit :bool) oneline (encoding :arg=)
  (notes :arg= :bool) (show-notes :flag :arg=) standard-notes show-signature
  (patch :bool) (unified :arg=) raw patch-with-raw minimal patience histogram
  (diff-algorigthm (:member :default :myers :minimal :patience :histogram))
  (stat :arg=) numstat shortstat (dirstat :arg=) summary patch-with-stat
  (null (:name z) :flag) name-only name-status (submodule :flag :arg=)
  (color :bool :arg=) (word-diff :flag :arg=) (word-diff-regex :arg=)
  (color-words :arg=) no-renames check (ws-error-highlight :arg=) full-index
  binary (abbrev :flag :arg=) (break-rewrites :flag :arg=) (find-renames :flag :arg=)
  (find-copies :flag :arg=) find-copies-harder irreversible-delete
  (limit-find (:name l) :arg.) (diff-filter :arg=)
  (differing-occurrences (:name S) :upcase :arg.)
  (differing-diffs (:name G) :upcase :arg.)
  pickaxe-all pickaxe-regex (order (:name O) :upcase :arg.)
  (swap (:name R) :upcase :flag) (relative :arg=) text ignore-space-at-eol
  ignore-space-change ignore-all-space ignore-blank-lines (inter-hunk-context :arg=)
  function-context (ext-diff :bool) (textconv :bool) (ignore-submodules :flag :arg=)
  (src-prefix :arg=) (dst-prefix :arg=) no-prefix)

(define-git-wrapper git-show-ref
  &optional (patterns :--)
  &key head tags heads dereference (hash :flag :arg=) verify (abbrev :arg=) quiet
  (exclude-existing :flag :arg=))

(define-git-wrapper git-stash
  (action (:member :list :show :drop :pop :apply :branch :save :clear :create :store) :front)
  &optional message
  &key patch (keep-index :bool) include-untracked all quiet)

(define-git-wrapper git-status
  (paths :--)
  &key short branch porcelain long verbose (untracked-files :flag :arg=)
  (ignore-submodules :flag :arg=) ignored (null (:name z) :flag) (column :bool :arg=))

(define-git-wrapper git-submodule
  (action (:member :add :status :init :deinit :update :summary :foreach :sync) :front)
  &optional commit (paths :--) (repository :--) repository-path
  &key quiet (branch (:name b) :arg) force (name :arg) (reference :arg) (depth :arg)
  cached recursive init remote no-fetch rebase merge files (summary-limit :arg))

(define-git-wrapper git-svn
  (command (:member :init :fetch :clone :rebase :dcommit :tag :log :blame :find-rev
                    :set-tree :create-ignore :show-ignore :mmkdirs :commit-diff
                    :info :proplist :propget :show-externals :gc :reset) :front)
  &key (trunk :arg=) (tags :arg=) (branches :arg=) stdlayout no-metadata use-svm-props
  use-svnsync-props (rewrite-root :arg=) (rewrite-uuid :arg=) (username :arg=)
  (prefix :arg=) (ignore-paths :arg=) (include-paths :arg=) no-minimize-url localtime
  parent (log-window-size :arg=) preserve-empty-dirs (placeholder-filename :arg=)
  local no-rebase (commit-url :arg) (mergeinfo :arg=) interactive (message :arg)
  (tag :arg) (destination :arg=) (commit-utl :arg) parents (revision :arg=) verbose
  (limit :arg=) incremental show-commit oneline git-format before after
  (shared :tag (:member :false :true :umask :group :all :world :everybody))
  (template :arg=) stdin rmdir edit find-copies-harder (authors-file :arg=)
  (authors-prof :arg=) quiet merge (strategy :arg=) preserve-merges dry-run
  use-log-author add-author-from (id :arg) (svn-remote :arg) follow-parent)

(define-git-wrapper git-symbolic-ref
  name
  &optional ref
  &key delete quiet short (message (:name m) :arg))

(define-git-wrapper git-tag
  &optional tags commit object patterns
  &key annotate sign (local-user :arg=) force delete verify (lines (:name n) :arg.)
  list (sort :arg=) (column :bool :arg=) (contains :arg) (points-at :arg)
  (message :arg=) (file :arg=) (cleanup :arg=) (format :arg=))

(define-git-wrapper git-update-index
  &optional (files :--)
  &key add remove refresh (quiet (:name q) :flag) ignore-submodules unmerged
  ignore-missing (cacheinfo :arg) index-info (chmod :arg=) (assume-unchanged :bool)
  really-refresh (skip-worktree :bool) again unresolve info-only force-remove replace
  stdin verbose (index-version :arg) (null (:name z) :flag) (split-index :bool)
  (untracked-cacke :bool) force-untracked-cache)

(define-git-wrapper git-update-ref
  &optional ref newvalue oldvalue
  &key (message (:name m) :arg) (delete (:name d) :flag) no-deref stdin
  (null (:name z) :flag))

(define-git-wrapper git-update-server-info
  &key force)

(define-git-wrapper git-verify-pack
  (packs :--)
  &key verbose stat-only)

(define-git-wrapper git-write-tree
  &key missing-ok (prefix :arg=))
