(uiop:define-package #:40ants-doc-full/world
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection))
(in-package #:40ants-doc-full/world)


(defsection @world (:title "PAX World"
                    :ignore-words ("CI"
                                   "MGL-PAX"
                                   "JSON"
                                   "URL"
                                   "HTML"))
  "MGL-PAX supported a \"World\" which was a registry of documents, which can generate
  cross-linked HTML documentation pages for all the registered
  documents.

  But I decided to drop this feature for now, because usually build libraries documentation
  separately as part of their CI pipline.

  If somebody want's cross referencing between different libraries, then instead
  of building their docs simultaneously, I'd suggest to create an index of entities,
  provided by libraries and to store them as a JSON file along with a library documentation.

  This way it will be possible to enumerate such sources of cross references as usual URLs.

  Such feature is not implemented in the 40ANTS-DOC system yet, but probably it will be
  useful for libraries built around the [Weblocks](https://40ants.com/weblocks/).
  If you want to help and implement the feature, please, let me know.")
