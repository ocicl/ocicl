(uiop:define-package #:40ants-doc-full/plugins/highlightjs
  (:use #:cl)
  (:import-from #:40ants-doc-full/themes/api
                #:copy-static
                #:inject-into-page-header)
  (:import-from #:spinneret
                #:with-html-string)
  (:import-from #:40ants-doc-full/utils
                #:make-relative-path)
  (:import-from #:dexador)
  (:import-from #:log)
  (:import-from #:alexandria
                #:when-let
                #:write-string-into-file
                #:write-byte-vector-into-file
                #:read-file-into-string)
  (:import-from #:tmpdir
                #:with-tmpdir)
  ;; TODO: Should be returned back after this issue resolve:
  ;; https://github.com/highlightjs/highlight.js/issues/3835
  ;; (:import-from #:trivial-extract
  ;;               #:extract-zip)
  (:import-from #:which)
  (:import-from #:jonathan)
  (:export #:highlightjs))
(in-package #:40ants-doc-full/plugins/highlightjs)


(defvar *default-languages*
  '("lisp" "bash" "css" "json" "yaml" "plaintext" "xml" "markdown"))


(defvar *default-theme*
  "atom-one-dark")


(defclass highlightjs ()
  ((languages :initform *default-languages*
              :initarg :languages
              :reader highlight-languages)
   (theme :initform *default-theme*
          :initarg :theme
          :reader highlight-theme))
  (:documentation "Injects a necessary scripts to use Highlightjs for rendering math formulas in your documentation."))


(defun highlightjs (&key (languages *default-languages*)
                         (theme *default-theme*))
  "Creates a Highlightjs plugin.

   You can redefine languages list and color theme like this:

   ```
   (make-instance '40ants-doc-full/themes/light:light-theme
                  :plugins (list
                            (highlightjs :theme \"magula\"
                                         :languages '(\"lisp\" \"python\"))))
   ```

"
  (make-instance 'highlightjs
                 :languages languages
                 :theme theme))


(defmethod inject-into-page-header ((plugin highlightjs) uri)
  (with-html-string
    (let ((highlight-css-uri (make-relative-path uri "highlight.min.css"))
          (highlight-js-uri (make-relative-path uri "highlight.min.js")))
      (:link :rel "stylesheet"
             :type "text/css"
             :href highlight-css-uri)
      (:script :type "text/javascript"
               :src highlight-js-uri)
      (:script :type "text/javascript"
               "hljs.highlightAll();"))))


(defvar *supported-languages*
  '("1c"
    "abnf"
    "accesslog"
    "actionscript"
    "ada"
    "angelscript"
    "apache"
    "applescript"
    "arcade"
    "arduino"
    "armasm"
    "asciidoc"
    "aspectj"
    "autohotkey"
    "autoit"
    "avrasm"
    "awk"
    "axapta"
    "bash"
    "basic"
    "bnf"
    "brainfuck"
    "c"
    "cal"
    "capnproto"
    "ceylon"
    "clean"
    "clojure-repl"
    "clojure"
    "cmake"
    "coffeescript"
    "coq"
    "cos"
    "cpp"
    "crmsh"
    "crystal"
    "csharp"
    "csp"
    "css"
    "d"
    "dart"
    "delphi"
    "diff"
    "django"
    "dns"
    "dockerfile"
    "dos"
    "dsconfig"
    "dts"
    "dust"
    "ebnf"
    "elixir"
    "elm"
    "erb"
    "erlang-repl"
    "erlang"
    "excel"
    "fix"
    "flix"
    "fortran"
    "fsharp"
    "gams"
    "gauss"
    "gcode"
    "gherkin"
    "glsl"
    "gml"
    "go"
    "golo"
    "gradle"
    "groovy"
    "haml"
    "handlebars"
    "haskell"
    "haxe"
    "hsp"
    "http"
    "hy"
    "inform7"
    "ini"
    "irpf90"
    "isbl"
    "java"
    "javascript"
    "jboss-cli"
    "json"
    "julia-repl"
    "julia"
    "kotlin"
    "lasso"
    "latex"
    "ldif"
    "leaf"
    "less"
    "lisp"
    "livecodeserver"
    "livescript"
    "llvm"
    "lsl"
    "lua"
    "makefile"
    "markdown"
    "mathematica"
    "matlab"
    "maxima"
    "mel"
    "mercury"
    "mipsasm"
    "mizar"
    "mojolicious"
    "monkey"
    "moonscript"
    "n1ql"
    "nestedtext"
    "nginx"
    "nim"
    "nix"
    "node-repl"
    "nsis"
    "objectivec"
    "ocaml"
    "openscad"
    "oxygene"
    "parser3"
    "perl"
    "pf"
    "pgsql"
    "php-template"
    "php"
    "plaintext"
    "pony"
    "powershell"
    "processing"
    "profile"
    "prolog"
    "properties"
    "protobuf"
    "puppet"
    "purebasic"
    "python-repl"
    "python"
    "q"
    "qml"
    "r"
    "reasonml"
    "rib"
    "roboconf"
    "routeros"
    "rsl"
    "ruby"
    "ruleslanguage"
    "rust"
    "sas"
    "scala"
    "scheme"
    "scilab"
    "scss"
    "shell"
    "smali"
    "smalltalk"
    "sml"
    "sqf"
    "sql"
    "stan"
    "stata"
    "step21"
    "stylus"
    "subunit"
    "swift"
    "taggerscript"
    "tap"
    "tcl"
    "thrift"
    "tp"
    "twig"
    "typescript"
    "vala"
    "vbnet"
    "vbscript-html"
    "vbscript"
    "verilog"
    "vhdl"
    "vim"
    "wasm"
    "wren"
    "x86asm"
    "xl"
    "xml"
    "xquery"
    "yaml"
    "zephir"))


(defun to-downcased-string (thing)
  (string-downcase
   (etypecase thing
     (symbol (symbol-name thing))
     (string thing))))

(defun normalize (lang)
  (let ((result (to-downcased-string lang)))
    (unless (member result *supported-languages* :test #'string=)
      (error "Language \"~A\" is not supported by highlight.js"
             result))
    result))

(defun normalize-langs (languages)
  (let* ((languages (if (eql languages :all)
                        *supported-languages*
                        (uiop:ensure-list languages)))
         (normalized (mapcar #'normalize languages))
         (sorted (sort normalized
                       #'string<)))
    sorted))

(defun generate-meta-data (languages theme)
  (format nil "languages: ~{~a~^, ~}~%theme: ~A~%"
          languages
          (to-downcased-string theme)))

(defun extract-zip-ignoring-error (pathname)
  (let ((binary (which:which "unzip")))
    (unless binary
      (error "Please unstall \"unzip\" utility."))
    
    (handler-bind ((uiop:subprocess-error
                     (lambda (e)
                       ;; Starting from 2023-08-05 Highlight.js started to use absolute
                       ;; pathnames in the zip archive. Here we are ignoring the error
                       ;; returned by unzip. And we'll have to do this until this issue
                       ;; will be resolved:
                       ;; https://github.com/highlightjs/highlight.js/issues/3835
                       (when (= (uiop:subprocess-error-code e) 1)
                         (invoke-restart
                          (find-restart 'continue))))))
      (uiop:run-program (format nil "~S -o ~S -d ~S"
                                (namestring binary)
                                (namestring pathname)
                                (namestring (uiop:pathname-directory-pathname pathname)))))))


(defun download-highlight-js (languages &key (to "./")
                                             (theme "default"))
  (with-tmpdir (tmpdir)
    (let* ((languages (normalize-langs languages))
           (to (uiop:ensure-directory-pathname to))
           (metadata-path (uiop:merge-pathnames* "METADATA"
                                                 to))
           (metadata (generate-meta-data languages theme)))

      (cond
        ((and (probe-file metadata-path)
              (string= (read-file-into-string metadata-path)
                       metadata))
         (log:info "METADATA file lists same languages and theme. Skipping download of Highlight.js"))
        (t
         (log:info "Downloading Highlight.js")
         (let* ((url "https://highlightjs.org/api/download")
                (post-data (list :|api| 2
                                 :|languages|
                                 (loop for lang in languages
                                       for normalized-lang = (normalize lang)
                                       collect normalized-lang)))
                (headers (list (cons "Content-Type" "application/json")))
                (response (dex:post url
                                    :content (jonathan:to-json post-data)
                                    :headers headers))
                (path (uiop:merge-pathnames* #P"archive.zip" tmpdir)))

           (ensure-directories-exist path)
           (ensure-directories-exist to)
           
           (write-byte-vector-into-file response path
                                        :if-exists :supersede)
           (extract-zip-ignoring-error path)

           (uiop:copy-file (uiop:merge-pathnames* "highlight.min.js" tmpdir)
                           (uiop:merge-pathnames* "highlight.min.js" to))

           (let* ((theme (to-downcased-string theme))
                  (theme-path (uiop:merge-pathnames* (format nil "styles/~A.min.css" theme)
                                                     tmpdir)))
             (unless (probe-file theme-path)
               (error "Theme \"~A\" was is not supported by Highlight.js"
                      theme))
             (uiop:copy-file theme-path
                             (uiop:merge-pathnames* "highlight.min.css" to)))

           (write-string-into-file metadata metadata-path
                                   :if-exists :supersede))))))
  (values))


(defmethod copy-static ((plugin highlightjs) target-dir)
  (download-highlight-js (highlight-languages plugin)
                         :to target-dir
                         :theme (highlight-theme plugin)))
