(in-package :vieltoenigkeit)

(defparameter *mei* (with-open-file (xml-stream "xml/01-mei.xml")
                      (xmls:parse xml-stream)))


'((:metadata (:title "Per non mi dir ch'io moia" :composer "Rossi"))
  (:music ((1 (:i :ottava :➚) (:s :semibrevis) (:i :quarta :➘)))))

'((:metadata (:title "Per non mi dir ch'io moia" :composer "Rossi"))
  (:music ((1 (:e♯ 5 :semibrevis)))))

(defun follow-path (mei-data name-strings)
  (if (null name-strings)
      (xmls:node-children mei-data)
      (let ((first-candidate (find (first name-strings)
                                   (xmls:node-children mei-data)
                                   :key #'xmls:node-name
                                   :test #'string=)))
        (if first-candidate
            (follow-path first-candidate (rest name-strings))
            nil))))

(defun get-child-by-name (mei-data name)
  (find name (xmls:node-children mei-data) :key #'xmls:node-name :test #'string=))

(defun read-attribute (xmls-node attr-name)
  (second (find attr-name (xmls:node-attrs xmls-node) :key #'first :test #'string=)))

(defun read-title (mei-document)
  (first (follow-path mei-document '("meiHead" "fileDesc" "titleStmt" "title"))))

(defun read-composer (mei-document)
  (first (follow-path mei-document '("meiHead" "fileDesc" "titleStmt" "respStmt" "persName"))))

(defun extract-voice-ns (mei-document)
  (mapcar (lambda (staff-node)
            (read-attribute staff-node "n"))
          (follow-path mei-document '("music" "body" "mdiv" "score" "scoreDef" "staffGrp"))))

(defun extract-measures (mei-document)
  (follow-path mei-document '("music" "body" "mdiv" "score" "section")))

(defun extract-notes-and-rests (measure-node staff-n)
  (follow-path (find staff-n (xmls:node-children measure-node)
                     :key (lambda (staff-node) (read-attribute staff-node "n"))
                     :test #'string=)
               '("layer")))

(defun convert-notes-and-rests (note-or-rest-node)
  (cond ((string= (xmls:node-name note-or-rest-node) "note")
         (list :note
               (read-attribute note-or-rest-node "pname")
               (when (get-child-by-name note-or-rest-node "accid")
                 (read-attribute (get-child-by-name note-or-rest-node "accid") "accid"))
               (read-attribute note-or-rest-node "oct")
               (read-attribute note-or-rest-node "dur")))
        ((string= (xmls:node-name note-or-rest-node) "rest")
         (list :rest
               (read-attribute note-or-rest-node "dur")))))

(defun read-music (mei-document)
  (let ((voice-ns (extract-voice-ns mei-document))
        (measures (extract-measures mei-document)))
    (mapcar (lambda (voice-n)
              (append (list voice-n)
                    (mapcar #'convert-notes-and-rests
                            (alexandria:mappend (lambda (measure)
                                                  (extract-notes-and-rests measure voice-n))
                                                measures))))
            voice-ns)))

(defun read-mei (mei-document)
  (if (string= (xmls:node-name mei-document) "mei")
      `((:metadata (:title ,(read-title mei-document)
                    :composer ,(read-composer mei-document)))
        (:music ,(read-music mei-document)))
      (error "No MEI document.")))
