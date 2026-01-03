;;;; -*- mode: lisp; indent-tabs-mode: nil -*-
(in-package :crypto-tests)

#.(loop for mac in (crypto:list-all-macs)
        collect `(rtest:deftest ,mac
                   (run-test-vector-file ',mac *mac-tests*) t)
          into forms
        finally (return `(progn ,@forms)))

#.(loop for mac in (crypto:list-all-macs)
        collect `(rtest:deftest ,(crypto::symbolicate mac '#:/incremental)
                   (run-test-vector-file ',mac *mac-incremental-tests*) t)
          into forms
        finally (return `(progn ,@forms)))

#.(if (boundp '*mac-stream-tests*)
      (loop for mac in (crypto:list-all-macs)
         collect `(rtest:deftest ,(crypto::symbolicate mac '#:/stream)
                      (run-test-vector-file ',mac *mac-stream-tests*) t)
           into forms
         finally (return `(progn ,@forms)))
      nil)

#.(loop for mac in (crypto:list-all-macs)
        collect `(rtest:deftest ,(crypto::symbolicate mac '#:/reinitialize-instance)
                   (run-test-vector-file ',mac *mac-reinitialize-instance-tests*) t)
          into forms
        finally (return `(progn ,@forms)))
