(asdf:defsystem "vieltoenigkeit"
  :depends-on (:drawer :vicentino-tunings :xmls)
  :serial t
  :components ((:file "package")
               (:file "xml-parsing")
               (:file "vieltoenigkeit-generator")))
