(in-package :vieltoenigkeit)

(defparameter *mei* (with-open-file (xml-stream "xml/01-mei.xml")
                      (xmls:parse xml-stream)))


;;; MEI/XML handling

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

;;; Generating raw score info, (:note [pname] [accid] [oct] [dur]) -> (:note "a" "s" "2" "1")

(defun read-mei (mei-document)
  "Returns score information in the form of 'score-expression', which is a proper list."
  (if (string= (xmls:node-name mei-document) "mei")
      `((:metadata (:title ,(read-title mei-document)
                    :composer ,(read-composer mei-document)))
        (:music ,(read-music mei-document)))
      (error "No MEI document.")))




;;; Converting score info, (:note "a" "s" "2" "1") -> (:note :a♯ 2 :semibrevis)

(defparameter *accid-dict* '(("s" . "♯")
                             ("f" . "♭")))

(defun lookup-accid (accid-string)
  (cdr (assoc accid-string *accid-dict* :test #'string=)))

(defparameter *dur-dict* '(("breve" . :brevis)
                           ("1" . :semibreivs)
                           ("2" . :minima)
                           ("4" . :semiminima)
                           ("8" . :fusa)
                           ("16" . :semifusa)))

(defun lookup-dur (dur-string)
  (cdr (assoc dur-string *dur-dict* :test #'string=)))

(defun translate-note (note-expression)
  (list :note (alexandria:make-keyword (format nil
                                               "~a~@[~a~]"
                                               (string-upcase (second note-expression))
                                               (lookup-accid (third note-expression))))
        (parse-integer (fourth note-expression))
        (lookup-dur (fifth note-expression))))

(defun translate-rest (rest-expression)
  (list :rest (lookup-dur (second rest-expression))))

(defun translate-note-or-rest (note-or-rest)
  (case (first note-or-rest)
    (:note (translate-note note-or-rest))
    (:rest (translate-rest note-or-rest))))

(defun translate-pitch-info (score-expression)
  "Returns a score expression (proper list) with converted expressions for notes and rests."
  (list (first score-expression)
        (list :music
              (mapcar (lambda (voice)
                        (append (list (first voice))
                                (mapcar (lambda (note-or-rest)
                                          (translate-note-or-rest note-or-rest))
                                        (rest voice))))
                      (second (second score-expression))))))


;;; Converting to relative intervals, (:note :a♯ 2 :semibrevis) -> (:i :tono :➚) (:s :semibrevis)

;; e b d c♯ c

(defparameter *relative-intervals-dict*
  '(((:e :b) ((0 (:quinta :➚ 0))
              (1 (:duodecii :➚))))))

(defun lookup-relative-interval (origin target)
  (let ((octave-delta (- (third target) (third origin))))
    ))

(defun add-to-interval-path (origin-note new-note)
  (case (first new-note)
    (:rest (list (list :t (second new-note))))
    (:note (list (append (list :i)
                         (lookup-relative-interval origin-note new-note))
                 (list :s (fourth new-note))))))

(defun converto-to-relative (score-expression origin-note)
  (let ((origin origin-note))
    (list (first score-expression)
          (list :music
                (mapcar (lambda (voice)
                          (append (list (first voice))
                                  (alexandria:mappend
                                   (lambda (note-or-rest)
                                     (prog1
                                         (add-to-interval-path origin note-or-rest)
                                       (when (eq (first note-or-rest) :note)
                                         (setf origin note-or-rest))))
                                                      (rest voice))))
                        (second (second score-expression)))))))
