(in-package :vieltoenigkeit)


;;; MEI/XML handling

(defun parse-mei (mei-filename)
  (with-open-file (mei-stream mei-filename)
    (xmls:parse mei-stream)))

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
  (when (xmls:node-p xmls-node)
    (second (find attr-name (xmls:node-attrs xmls-node) :key #'first :test #'string=))))

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

(defun note-tie-startid-p (note-id ties)
  (find note-id ties :key #'car :test #'string=))

(defun note-tie-endid-p (note-id ties)
  (find note-id ties :key #'cdr :test #'string=))

(defun locate-node-with-id (id all-nodes)
  (block search
    (dolist (voice all-nodes)
      (dolist (candidate (rest voice))
        (when (string= (read-attribute candidate "id") id)
          (return-from search candidate))))))

(defun parse-dots (note-or-rest-node)
  (if (read-attribute note-or-rest-node "dots")
      (parse-integer (read-attribute note-or-rest-node "dots"))
      0))

(defun convert-notes-and-rests (note-or-rest-node all-nodes ties)
  (let ((this-id (read-attribute note-or-rest-node "id")))
    (cond ((string= (xmls:node-name note-or-rest-node) "note")
           (let ((duration-list (list (cons (read-attribute note-or-rest-node "dur")
                                            (parse-dots note-or-rest-node))))
                 (accidental (when (get-child-by-name note-or-rest-node "accid")
                               (read-attribute (get-child-by-name note-or-rest-node "accid")
                                               "accid"))))
             (labels ((collect-tied-notes (local-start-id)
                        (when (note-tie-startid-p local-start-id ties)
                          (let ((end-note (locate-node-with-id (cdr (note-tie-startid-p
                                                                     local-start-id ties))
                                                               all-nodes)))
                            (push (cons (read-attribute end-note "dur") (parse-dots end-note))
                                  duration-list)
                            (collect-tied-notes (read-attribute end-note "id"))))))
               (collect-tied-notes this-id))
             (when (and (null accidental)
                        (note-tie-endid-p this-id ties))
               (setf accidental
                     (read-attribute (get-child-by-name
                                      (locate-node-with-id (car (note-tie-endid-p this-id ties))
                                                           all-nodes)
                                      "accid")
                                     "accid")))
             (unless (note-tie-endid-p this-id ties)
               (list :note
                     (read-attribute note-or-rest-node "pname")
                     accidental
                     (read-attribute note-or-rest-node "oct")
                     duration-list))))
          ((string= (xmls:node-name note-or-rest-node) "rest")
           (list :rest (list (cons (read-attribute note-or-rest-node "dur")
                                   (parse-dots note-or-rest-node))))))))

(defun extract-ties (mei-document)
  (let ((collector '()))
    (labels ((tree-loop (mei-element)
               (unless (null mei-element)
                 (if (string= (xmls:node-name mei-element) "tie")
                     (push (cons (subseq (read-attribute mei-element "startid") 1)
                                 (subseq (read-attribute mei-element "endid") 1))
                           collector)
                     (dolist (child (xmls:node-children mei-element))
                       (when (xmls:node-p child) (tree-loop child)))))))
      (tree-loop mei-document))
    collector))

(defun read-music (mei-document)
  (let ((voice-ns (extract-voice-ns mei-document))
        (measures (extract-measures mei-document))
        (ties (extract-ties mei-document)))
    (let ((note-and-rest-nodes
            (mapcar (lambda (voice-n)
                      (append (list voice-n)
                              (alexandria:mappend (lambda (measure)
                                                    (extract-notes-and-rests measure
                                                                             voice-n))
                                                  measures)))
                    voice-ns)))
      (mapcar (lambda (voice)
                (append (list (first voice))
                        (remove-if #'null (mapcar (lambda (note-or-rest)
                                                    (convert-notes-and-rests note-or-rest
                                                                             note-and-rest-nodes
                                                                             ties))
                                                  (rest voice)))))
              note-and-rest-nodes))))

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
                           ("1" . :semibrevis)
                           ("2" . :minima)
                           ("4" . :semiminima)
                           ("8" . :fusa)
                           ("16" . :semifusa)))

(defun lookup-dur (dur-string)
  (cdr (assoc dur-string *dur-dict* :test #'string=)))

(defun lookup-dur-list (dur-pair-list)
  (mapcar (lambda (dur-pair)
            (cons (lookup-dur (car dur-pair))
                  (cdr dur-pair)))
          dur-pair-list))

(defun make-note-name-keyword (note-expression)
  (alexandria:make-keyword
   (if (and (string= (second note-expression) "b")
            (null (third note-expression)))
       "B♮"
       (format nil
               "~a~@[~a~]"
               (string-upcase (second note-expression))
               (lookup-accid (third note-expression))))))

(defun translate-note (note-expression)
  (list :note
        (make-note-name-keyword note-expression)
        (parse-integer (fourth note-expression))
        (lookup-dur-list (fifth note-expression))))

(defun translate-rest (rest-expression)
  (list :rest (lookup-dur-list (second rest-expression))))

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
  '((:unisono ((:c :c) (:d :d) (:e :e) (:f :f) (:g :g) (:a :a) (:b♮ :b♮) (:b♭ :b♭)
               (:c♯ :c♯) (:e♭ :e♭) (:f♯ :f♯) (:g♯ :g♯) (:b♮ :b♮) (:b♭ :b♭)
               (:d♭ :d♭) (:d♯ :d♯) (:e♯ :e♯) (:g♭ :g♭) (:a♭ :a♭) (:a♯ :a♯)))
    (:diesis ((:c♯ :d♭) (:f♯ :g♭) (:g♯ :a♭)
              (:d♯ :e♭) (:e♯ :f) (:a♯ :b♭) (:b♯ :c 1)))
    (:semitono-minore ((:c :c♯) (:d :d♯) (:e :e♯) (:f :f♯) (:g :g♯) (:a :a♯) (:b♮ :b♯) (:b♭ :b♮)
                       (:e♭ :e) (:b♭ :b♮)
                       (:d♭ :d) (:g♭ :g) (:a♭ :a)
                       (:c♭ :c) (:f♭ :f)))
    (:semitono-maggiore ((:c :d♭) (:d :e♭) (:e :f) (:f :g♭) (:g :a♭) (:a :b♭) (:b♭ :c♭ 1)
                         (:b♮ :c 1)
                         (:c♯ :d) (:e♭ :f♭) (:f♯ :g) (:g♯ :a)
                         (:d♯ :e) (:e♯ :f♯) (:a♯ :b♮) (:b♯ :c♯ 1)))
    (:tono ((:c :d) (:d :e) (:e :f♯) (:f :g) (:g :a) (:a :b♮) (:b♮ :c♯ 1) (:b♭ :c 1)
            (:c♯ :d♯) (:e♭ :f) (:f♯ :g♯) (:g♯ :a♯)
            (:d♭ :e♭) (:d♯ :e♯) (:g♭ :a♭) (:a♭ :b♭) (:a♯ :b♯)
            (:c♭ :d♭) (:f♭ :g♭)))
    (:tono-maggiore ((:c♯ :e♭) (:f♯ :a♭) (:g♯ :b♭) (:b♮ :d♭ 1)
                     (:d♯ :f) (:a♯ :c 1)))
    (:terza-minima ((:c :d♯) (:d :e♯) (:f :g♯) (:g :a♯) (:a :b♯) (:b♭ :c♯ 1)
                    (:e♭ :f♯)
                    (:d♭ :e) (:g♭ :a) (:a♭ :b♮)))
    (:terza-minore ((:c :e♭) (:d :f) (:e :g) (:f :a♭) (:g :b♭) (:a :c 1) (:b♮ :d 1) (:b♭ :d♭ 1)
                    (:c♯ :e) (:e♭ :g♭) (:f♯ :a) (:g♯ :b♮)
                    (:d♭ :f♭) (:d♯ :f♯) (:e♯ :g♯) (:a♭ :c♭) (:a♯ :c♯ 1)))
    (:terza-maggiore ((:c :e) (:d :f♯) (:e :g♯) (:f :a) (:g :b♮) (:a :c♯ 1) (:b♮ :d♯ 1) (:b♭ :d 1)
                      (:c♯ :e♯) (:e♭ :g) (:f♯ :a♯) (:g♯ :b♯)
                      (:d♭ :f) (:g♭ :b♭) (:a♭ :c 1)
                      (:c♭ :e♭) (:f♭ :a♭)))
    (:terza-maggiore-propinqua ((:c :f♭) (:d :g♭) (:e :a♭) (:g :c♭ 1) (:a :d♭ 1) (:b♮ :e♭ 1)
                                (:c♯ :f) (:f♯ :b♭) (:g♯ :c 1)
                                (:d♯ :g) (:e♯ :a) (:a♯ :d 1)))
    (:quarta ((:c :f) (:d :g) (:e :a) (:f :b♭) (:g :c 1) (:a :d 1) (:b♮ :e 1) (:b♭ :e♭ 1)
              (:c♯ :f♯) (:e♭ :a♭) (:f♯ :b♮) (:g♯ :c♯ 1)
              (:d♭ :g♭) (:d♯ :g♯) (:e♯ :a♯) (:g♭ :c♭ 1) (:a♭ :d♭ 1) (:a♯ :d♯ 1) (:b♯ :e♯ 1)))
    (:tritono ((:c :f♯) (:d :g♯) (:e :a♯) (:f :b♮) (:g :c♯ 1) (:a :d♯ 1) (:b♮ :e♯ 1) (:b♭ :e 1)))
    (:quinta-imperfetta ((:c :g♭) (:d :a♭) (:e :b♭) (:f :c♭ 1) (:g :d♭ 1) (:a :e♭) (:b♮ :f 1)
                         (:b♭ :f♭ 1)
                         (:c♯ :g) (:f♯ :c 1) (:g♯ :d 1)))
    (:quinta ((:c :g) (:d :a) (:e :b♮)
              (:f :c 1) (:g :d 1) (:a :e 1) (:b♭ :f 1) (:b♮ :f♯ 1)
              (:c♯ :g♯) (:e♭ :b♭) (:f♯ :c♯ 1) (:g♯ :d♯ 1)))
    (:quinta-propinqua ((:c♯ :a♭) (:f♯ :d♭ 1) (:g♯ :e♭ 1)
                        (:d♯ :b♭) (:e♯ :c 1) (:a♯ :f 1) (:b♯ :g 1)))
    (:sesta-minima ((:e♭ :b♮)))
    (:sesta-minore ((:c :a♭) (:d :b♭) (:e :c 1) (:f :d♭ 1) (:g :e♭ 1) (:a :f 1)
                    (:b♮ :g 1) (:b♭ :g♭ 1)))
    (:sesta-maggiore ((:c :a) (:d :b♮) (:e :c♯ 1) (:f :d 1) (:g :e 1) (:a :f♯ 1) (:b♮ :g♯ 1)
                      (:b♭ :g 1)
                      (:c♯ :a♯) (:e♭ :c♭ 1) (:f♯ :d♯ 1) (:g♯ :e♯ 1)))
    (:settima-minore ((:c :b♭) (:d :c 1) (:e :d 1) (:f :e♭ 1) (:g :f 1) (:a :g 1) (:b♮ :a 1)
                      (:b♭ :a♭ 1)
                      (:c♯ :b♮) (:e♭ :d♭ 1) (:f♯ :e 1) (:g♯ :f♯ 1)))))

(defmacro toward-zero (num)
  `(setf ,num (- ,num (signum ,num))))

(defun lookup-relative-interval (origin target)
  (block search
    (loop for i from (abs (- (third target) (third origin))) to 0
          for octave-delta = (- (third target) (third origin))
          for multiplier from 0
          do (let ((search-pattern (list (second origin)
                                         (second target)
                                         octave-delta)))
               (dolist (interval *relative-intervals-dict*)
                 (dolist (pattern (second interval))
                   (let ((complete-pattern (if (third pattern)
                                               pattern
                                               (list (first pattern) (second pattern) 0))))
                     (when (equalp complete-pattern search-pattern)
                       (return-from search (list (first interval) :➚)))
                     (when (equalp complete-pattern (list (second search-pattern)
                                                          (first search-pattern)
                                                          (- (third search-pattern))))
                       (return-from search (list (first interval) :➘ multiplier)))))))
             (toward-zero octave-delta))
    (error "Interval between ~a and ~a not known." origin target)))

(defun lookup-relative-interval (origin target)
  (block search
    (do ((i (abs (- (third target) (third origin))) (1- i))
         (octave-delta (- (third target) (third origin)) (toward-zero octave-delta))
         (multiplier 0 (1+ multiplier)))
        ((minusp i) (error "Interval between ~a and ~a not known." origin target))
      (let ((search-pattern (list (second origin)
                                  (second target)
                                  octave-delta)))
        ;; (format t "~&Round 1, i=~a, octave-delta=~a, multiplier=~a." i octave-delta multiplier)
        (dolist (interval *relative-intervals-dict*)
          (dolist (pattern (second interval))
            (let ((complete-pattern (if (third pattern)
                                        pattern
                                        (list (first pattern) (second pattern) 0))))
              (when (equalp complete-pattern search-pattern)
                (return-from search (list (first interval) :➚ multiplier)))
              (when (equalp complete-pattern (list (second search-pattern)
                                                   (first search-pattern)
                                                   (- (third search-pattern))))
                (return-from search (list (first interval) :➘ multiplier))))))))))

(defun add-to-interval-path (origin-note new-note)
  (case (first new-note)
    (:rest (list (list :t (second new-note))))
    (:note (list (append (list :i)
                         (lookup-relative-interval origin-note new-note))
                 (list :s (fourth new-note))))))

(defun convert-to-relative (score-expression origin-note)
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


(defun relative-score (mei-file-name)
  (convert-to-relative (translate-pitch-info (read-mei (parse-mei mei-file-name)))
                       '(:note :c 3)))


(defparameter *score-1* (relative-score "xml/01-mei.xml"))


;;; Drawing scores

(defparameter *duration-dict* '((:brevis . 2)
                                (:semibrevis . 1)
                                (:minima . 1/2)
                                (:semiminima . 1/4)
                                (:fusa . 1/8)
                                (:semifusa . 1/16)))

(defun lookup-duration (duration-list)
  (reduce #'+ (mapcar (lambda (duration)
                        (let ((result (cdr (assoc (car duration) *duration-dict*))))
                          (dotimes (i (cdr duration) result)
                            (incf result (* result (/ 1 (+ 2 i)))))))
                      duration-list)))

(defparameter *interval-size-dict*
  `((:unisono . 0)
    (:diesis . ,(vicentino-tunings:interval-size :tuning1 :d♯ :up :e♭))
    (:semitono-minore . ,(vicentino-tunings:interval-size :tuning1 :c :up :c♯))
    (:semitono-maggiore . ,(vicentino-tunings:interval-size :tuning1 :e :up :f))
    (:tono . ,(vicentino-tunings:interval-size :tuning1 :c :up :d))
    (:tono-maggiore . ,(vicentino-tunings:interval-size :tuning1 :c♯ :up :e♭))
    (:terza-minima . ,(vicentino-tunings:interval-size :tuning1 :c :up :d♯))
    (:terza-minore . ,(vicentino-tunings:interval-size :tuning1 :d :up :f))
    (:terza-maggiore . ,(vicentino-tunings:interval-size :tuning1 :c :up :e))
    (:terza-maggiore-propinqua . ,(vicentino-tunings:interval-size :tuning1 :e :up :a♭))
    (:quarta . ,(vicentino-tunings:interval-size :tuning1 :c :up :f))
    (:tritono . ,(vicentino-tunings:interval-size :tuning1 :c :up :f♯))
    (:quinta-imperfetta . ,(vicentino-tunings:interval-size :tuning1 :d :up :a♭))
    (:quinta . ,(vicentino-tunings:interval-size :tuning1 :d :up :a))
    (:sesta-minore . ,(vicentino-tunings:interval-size :tuning1 :d :up :b♭))
    (:sesta-maggiore . ,(vicentino-tunings:interval-size :tuning1 :c :up :a))
    (:settima-minore . ,(vicentino-tunings:interval-size :tuning1 :c :up :b♭))))

(defun lookup-interval-size (interval-name direction)
  (* (if (eq :➚ direction) 1 -1)
     (cdr (assoc interval-name *interval-size-dict*))))

(defparameter *interval-fifth-index*
  `((:unisono . 0)
    (:diesis . 12)
    (:semitono-minore . 7)
    (:semitono-maggiore . -5)
    (:tono . 2)
    (:tono-maggiore . -10) ; really?
    (:terza-minima . 9) ; really?
    (:terza-minore . -3)
    (:terza-maggiore . 4)
    (:terza-maggiore-propinqua . -8) ; really?
    (:quarta . -1)
    (:tritono . 6)
    (:quinta-imperfetta . -6)
    (:quinta . 1)
    (:quinta-propinqua . 0) ; really?
    (:sesta-minore . -4)
    (:sesta-maggiore . 3)
    (:settima-minore . 2)))


(defun lookup-fifth-index (interval-name direction)
  (* (if (eq :➚ direction) 1 -1)
     (cdr (assoc interval-name *interval-fifth-index*))))

(defun calculate-score-duration (voice-data)
  (reduce #'+ (remove-if #'null (mapcar (lambda (item)
                                          (case (first item)
                                            (:s (lookup-duration (second item)))
                                            (:t (lookup-duration (second item)))))
                                        (rest voice-data)))))

(defun draw-voice (voice-data pitch-fun x-scale y-scale origin-note)
  (gr (list (let ((y (funcall pitch-fun (first origin-note) (second origin-note))))
              (ln (pt 0 y) (pt (* x-scale (calculate-score-duration voice-data)) y)
                  :style-update '(:line-type :dotted)))
            (gr (let ((time-cursor 0)
                      (pitch-cursor 0))
                  (remove-if #'null
                             (mapcar (lambda (item)
                                       (case (first item)
                                         (:t (incf time-cursor
                                                   (lookup-duration (second item)))
                                          nil)
                                         (:s (ln (pt (* x-scale time-cursor)
                                                     (* y-scale pitch-cursor))
                                                 (pt (* x-scale
                                                        (incf time-cursor
                                                              (lookup-duration (second item))))
                                                     (* y-scale pitch-cursor))
                                                 :style-update '(:line-type :thick)))
                                         (:i (ln (pt (* x-scale time-cursor)
                                                     (* y-scale pitch-cursor))
                                                 (pt (* x-scale time-cursor)
                                                     (* y-scale
                                                        (incf pitch-cursor
                                                              (funcall pitch-fun
                                                                       (second item)
                                                                       (third item)))))
                                                 :style-update '(:line-type :thin)))))
                                     (rest voice-data))))))))

(defun draw-score (relative-score filename
                   &key voice-label-filter (pitch-fun #'lookup-interval-size)
                     (x-scale 1) (y-scale 1/5) (origin-note '(:unisono :➚)) (y-padding 20))
  (let ((btikz (drawer:make-backend-tikz
                :filename (format nil "~a.tex" filename)
                :path (merge-pathnames "tikz/"
                                       (asdf/system:system-source-directory :vieltoenigkeit))))
        (voices (gr (let ((v-counter -1))
                      (mapcar (lambda (voice)
                                (cp voice (pt 0 0) (pt 0 (* (- (incf v-counter))
                                                            y-padding))))
                              (remove-if #'null
                                         (mapcar (lambda (voice)
                                                   (if voice-label-filter
                                                       (when (member (first voice) voice-label-filter
                                                                     :test #'string=)
                                                         (draw-voice voice pitch-fun x-scale y-scale origin-note))
                                                       (draw-voice voice pitch-fun x-scale y-scale origin-note)))
                                                 (second (second relative-score)))))))))
        (drawer:draw-with-multiple-backends (list btikz) (list voices))
    (drawer:compile-tikz btikz
                         (merge-pathnames "tikz/"
                                          (asdf/system:system-source-directory :vieltoenigkeit)))))


(defparameter *test* (make-hash-table :test #'equal))


;;; Doing simple statistics

(defun interval-stats (relative-score &key (sounding-filter nil))
  ;; TODO read metadata
  (let ((counter (make-hash-table :test #'equal))
        (soundingp nil))
    (dolist (voice (second (second relative-score)))
      (dolist (note-or-rest (rest voice))
        (when (eq :s (first note-or-rest)) (setf soundingp t))
        (when (eq :t (first note-or-rest)) (setf soundingp nil))
        (when (eq :i (first note-or-rest))
          (unless (gethash (rest note-or-rest) counter)
            (setf (gethash (rest note-or-rest) counter) 0))
          (if sounding-filter
              (when soundingp (incf (gethash (rest note-or-rest) counter)))
              (incf (gethash (rest note-or-rest) counter))))))
    counter))

(defun print-interval-stats (relative-score)
  (dolist (entry (sort (loop for interval being the hash-keys of (interval-stats relative-score :sounding-filter t)
                               using (hash-value number)
                             collect (cons interval number))
                       #'>
                       :key #'cdr))
    (unless (eq :unisono (first (car entry)))
      (format t
              "~&~a: ~a ~a ~@[(x~a)~]"
              (cdr entry)
              (first (car entry))
              (second (car entry))
              (when (plusp (third (car entry)))
                (third (car entry)))))))


(print-interval-stats *score-1*)
