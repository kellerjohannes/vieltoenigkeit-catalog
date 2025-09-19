(asdf:defsystem "vieltoenigkeit"
  :depends-on (:drawer :vicentino-tunings)
  :serial t
  :components ((:file "package")
               (:file "vieltoenigkeit-generator")))
