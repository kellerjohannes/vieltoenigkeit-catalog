\version "2.24.1"

#(ly:set-option 'crop #t)

% Auto generated file

\header {
  tagline = ##f
}

#(define ((fixed-signature-c-cut glyph) grob)
    (grob-interpret-markup grob
      (markup #:override '(baseline-skip . 0) #:number
        (markup (#:fontsize 0 #:musicglyph glyph)))))

dot = {
   \once \override Script.add-stem-support = ##f
   \once \override Script.toward-stem-shift = 0
   \once \override Script.skyline-horizontal-padding = 0
   \once \override Script.direction = 1
   \once \override Script.font-size = 1
}


diesis = \markup {
  \override #'(thickness . 1.7)
  \raise #-0.5 \draw-line #'(1 . 1)
  \hspace #-1.8
  \override #'(thickness . 1.7)
  \raise #-0.5 \draw-line #'(-1 . 1)
}


% small
diesisCromaticoSmall = \markup {
  \override #'(thickness . 1.7)
  \raise #-0.75 \draw-line #'(1.5 . 1.5)
  \hspace #-1.8
  \override #'(thickness . 1.7)
  \raise #-0.75 \draw-line #'(1.5 . 1.5)

  \hspace #-2.25
  \override #'(thickness . 1.7)
  \raise #-0.75 \draw-line #'(-1.5 . 1.5)
  \hspace #-2.73
  \override #'(thickness . 1.7)
  \raise #-0.75 \draw-line #'(-1.5 . 1.5)
}

% medium and thin
diesisCromatico = \markup {
  \override #'(thickness . 1.5)
  \raise #-0.62 \draw-line #'(1.2 . 1.2)
  \hspace #-1.55
  \override #'(thickness . 1.5)
  \raise #-0.62 \draw-line #'(1.2 . 1.2)

  \hspace #-2.25
  \override #'(thickness . 1.5)
  \raise #-0.62 \draw-line #'(-1.2 . 1.2)
  \hspace #-1.55
  \override #'(thickness . 1.5)
  \raise #-0.62 \draw-line #'(-1.2 . 1.2)
}

% large
diesisCromaticoLarge = \markup {
  \override #'(thickness . 1.7)
  \raise #-0.75 \draw-line #'(1.5 . 1.5)
  \hspace #-1.8
  \override #'(thickness . 1.7)
  \raise #-0.75 \draw-line #'(1.5 . 1.5)

  \hspace #-2.25
  \override #'(thickness . 1.7)
  \raise #-0.75 \draw-line #'(-1.5 . 1.5)
  \hspace #-2.73
  \override #'(thickness . 1.7)
  \raise #-0.75 \draw-line #'(-1.5 . 1.5)
}

\layout {
  \context {
    \Score
  }
}



\markup {
  \center-column {
    \line {
      \center-align
      \fontsize#3 { \concat {  \normal-text "Darstellung der diatonischen Hand" }  }
        \vspace #1
    }
    \null

    \line {
      \center-column {
        \line {
          \left-align { \concat {  \normal-text "Alamire" }  }
        }
        \vspace #0.2
        \line {

          \score {
            \new ChoirStaff
            <<
              \new Staff \with { instrumentName = ""}
              {
                \new Voice \with { \remove "Note_heads_engraver"
                                    \consists "Completion_heads_engraver"
                                    \remove "Rest_engraver"
                                    \consists "Completion_rest_engraver"}
                {
                  \sectionLabel \markup { \ellipse \fontsize #-8 "1" }
                  \accidentalStyle Score.forget
%                 \override Accidental.after-line-breaking = #'()
                  \override Rest.style = #'default
                  \override NoteHead.style = #'baroque
                  \cadenzaOn
                  \override Staff.TimeSignature.stencil = ##f

                  \clef "bass"
                  \key c #`((0 . ,NATURAL) (1 . ,NATURAL) (2 . ,NATURAL)
                            (3 . ,NATURAL) (4 . ,NATURAL) (5 . ,NATURAL)
                            (6 . ,NATURAL))  a,1   a,1   a,1
                  \cadenzaOff
                }
                \addlyrics { \set stanza = \markup{ \normal-text "per ɴ:" } la }                \addlyrics { \set stanza = \markup{ \normal-text "per ♭:" } \skip 1 mi }                \addlyrics { \set stanza = \markup{ \normal-text "per ♮:" } \skip 1 \skip 1 re }
              }
            >>
            \layout {
              \enablePolymeter
%             #(layout-set-staff-size 15)

              \context {
                \Score
                \override SpacingSpanner.common-shortest-duration = #(ly:make-moment 1)
                \override LyricText.font-size = #'-1.0
                \override Accidental.stencil =
                #(lambda (grob)
                (let ((alteration (ly:pitch-alteration
                                   (ly:event-property
                                    (ly:grob-property (ly:grob-property grob 'cause) 'cause)
                                    'pitch))))
                 (cond ((= 1/4 alteration) (grob-interpret-markup grob (markup diesis)))
                       ((= 1/2 alteration) (grob-interpret-markup grob (markup diesisCromatico)))
                       (else (ly:accidental-interface::print grob)))))
              }
              \context {
                \Voice
                \consists Horizontal_bracket_engraver
                \override HorizontalBracket.direction = #UP
              }
            }
          }

        }

      }
      \hspace #3
      \center-column {
        \line {
          \left-align { \concat {  \normal-text "Bfabmi" }  }
        }
        \vspace #0.2
        \line {

          \score {
            \new ChoirStaff
            <<
              \new Staff \with { instrumentName = ""}
              {
                \new Voice \with { \remove "Note_heads_engraver"
                                    \consists "Completion_heads_engraver"
                                    \remove "Rest_engraver"
                                    \consists "Completion_rest_engraver"}
                {
                  \sectionLabel \markup { \ellipse \fontsize #-8 "2" }
                  \accidentalStyle Score.forget
%                 \override Accidental.after-line-breaking = #'()
                  \override Rest.style = #'default
                  \override NoteHead.style = #'baroque
                  \cadenzaOn
                  \override Staff.TimeSignature.stencil = ##f

                  \clef "bass"
                  \key c #`((0 . ,NATURAL) (1 . ,NATURAL) (2 . ,NATURAL)
                            (3 . ,NATURAL) (4 . ,NATURAL) (5 . ,NATURAL)
                            (6 . ,FLAT))  bes,1   b,1
                  \cadenzaOff
                }
                \addlyrics { \set stanza = \markup{ \normal-text "per ♭:" } fa }                \addlyrics { \set stanza = \markup{ \normal-text "per ♮:" } \skip 1 mi }
              }
            >>
            \layout {
              \enablePolymeter
%             #(layout-set-staff-size 15)

              \context {
                \Score
                \override SpacingSpanner.common-shortest-duration = #(ly:make-moment 1)
                \override LyricText.font-size = #'-1.0
                \override Accidental.stencil =
                #(lambda (grob)
                (let ((alteration (ly:pitch-alteration
                                   (ly:event-property
                                    (ly:grob-property (ly:grob-property grob 'cause) 'cause)
                                    'pitch))))
                 (cond ((= 1/4 alteration) (grob-interpret-markup grob (markup diesis)))
                       ((= 1/2 alteration) (grob-interpret-markup grob (markup diesisCromatico)))
                       (else (ly:accidental-interface::print grob)))))
              }
              \context {
                \Voice
                \consists Horizontal_bracket_engraver
                \override HorizontalBracket.direction = #UP
              }
            }
          }

        }

      }
      \hspace #3
      \center-column {
        \line {
          \left-align { \concat {  \normal-text "Csolfaut" }  }
        }
        \vspace #0.2
        \line {

          \score {
            \new ChoirStaff
            <<
              \new Staff \with { instrumentName = ""}
              {
                \new Voice \with { \remove "Note_heads_engraver"
                                    \consists "Completion_heads_engraver"
                                    \remove "Rest_engraver"
                                    \consists "Completion_rest_engraver"}
                {
                  \sectionLabel \markup { \ellipse \fontsize #-8 "3" }
                  \accidentalStyle Score.forget
%                 \override Accidental.after-line-breaking = #'()
                  \override Rest.style = #'default
                  \override NoteHead.style = #'baroque
                  \cadenzaOn
                  \override Staff.TimeSignature.stencil = ##f

                  \clef "bass"
                  \key c #`((0 . ,NATURAL) (1 . ,NATURAL) (2 . ,NATURAL)
                            (3 . ,NATURAL) (4 . ,NATURAL) (5 . ,NATURAL)
                            (6 . ,NATURAL))  c1   c1   c1
                  \cadenzaOff
                }
                \addlyrics { \set stanza = \markup{ \normal-text "per ♭:" } sol }                \addlyrics { \set stanza = \markup{ \normal-text "per ♮:" } \skip 1 fa }                \addlyrics { \set stanza = \markup{ \normal-text "per ɴ:" } \skip 1 \skip 1 ut }
              }
            >>
            \layout {
              \enablePolymeter
%             #(layout-set-staff-size 15)

              \context {
                \Score
                \override SpacingSpanner.common-shortest-duration = #(ly:make-moment 1)
                \override LyricText.font-size = #'-1.0
                \override Accidental.stencil =
                #(lambda (grob)
                (let ((alteration (ly:pitch-alteration
                                   (ly:event-property
                                    (ly:grob-property (ly:grob-property grob 'cause) 'cause)
                                    'pitch))))
                 (cond ((= 1/4 alteration) (grob-interpret-markup grob (markup diesis)))
                       ((= 1/2 alteration) (grob-interpret-markup grob (markup diesisCromatico)))
                       (else (ly:accidental-interface::print grob)))))
              }
              \context {
                \Voice
                \consists Horizontal_bracket_engraver
                \override HorizontalBracket.direction = #UP
              }
            }
          }

        }

      }
      \hspace #3
      \center-column {
        \line {
          \left-align { \concat {  \normal-text "Dlasolre" }  }
        }
        \vspace #0.2
        \line {

          \score {
            \new ChoirStaff
            <<
              \new Staff \with { instrumentName = ""}
              {
                \new Voice \with { \remove "Note_heads_engraver"
                                    \consists "Completion_heads_engraver"
                                    \remove "Rest_engraver"
                                    \consists "Completion_rest_engraver"}
                {
                  \sectionLabel \markup { \ellipse \fontsize #-8 "4" }
                  \accidentalStyle Score.forget
%                 \override Accidental.after-line-breaking = #'()
                  \override Rest.style = #'default
                  \override NoteHead.style = #'baroque
                  \cadenzaOn
                  \override Staff.TimeSignature.stencil = ##f

                  \clef "bass"
                  \key c #`((0 . ,NATURAL) (1 . ,NATURAL) (2 . ,NATURAL)
                            (3 . ,NATURAL) (4 . ,NATURAL) (5 . ,NATURAL)
                            (6 . ,NATURAL))  d1   d1   d1
                  \cadenzaOff
                }
                \addlyrics { \set stanza = \markup{ \normal-text "per ♭:" } la }                \addlyrics { \set stanza = \markup{ \normal-text "per ♮:" } \skip 1 sol }                \addlyrics { \set stanza = \markup{ \normal-text "per ɴ:" } \skip 1 \skip 1 re }
              }
            >>
            \layout {
              \enablePolymeter
%             #(layout-set-staff-size 15)

              \context {
                \Score
                \override SpacingSpanner.common-shortest-duration = #(ly:make-moment 1)
                \override LyricText.font-size = #'-1.0
                \override Accidental.stencil =
                #(lambda (grob)
                (let ((alteration (ly:pitch-alteration
                                   (ly:event-property
                                    (ly:grob-property (ly:grob-property grob 'cause) 'cause)
                                    'pitch))))
                 (cond ((= 1/4 alteration) (grob-interpret-markup grob (markup diesis)))
                       ((= 1/2 alteration) (grob-interpret-markup grob (markup diesisCromatico)))
                       (else (ly:accidental-interface::print grob)))))
              }
              \context {
                \Voice
                \consists Horizontal_bracket_engraver
                \override HorizontalBracket.direction = #UP
              }
            }
          }

        }

      }
      \hspace #3
      \center-column {
        \line {
          \left-align { \concat {  \normal-text "Elami" }  }
        }
        \vspace #0.2
        \line {

          \score {
            \new ChoirStaff
            <<
              \new Staff \with { instrumentName = ""}
              {
                \new Voice \with { \remove "Note_heads_engraver"
                                    \consists "Completion_heads_engraver"
                                    \remove "Rest_engraver"
                                    \consists "Completion_rest_engraver"}
                {
                  \sectionLabel \markup { \ellipse \fontsize #-8 "5" }
                  \accidentalStyle Score.forget
%                 \override Accidental.after-line-breaking = #'()
                  \override Rest.style = #'default
                  \override NoteHead.style = #'baroque
                  \cadenzaOn
                  \override Staff.TimeSignature.stencil = ##f

                  \clef "bass"
                  \key c #`((0 . ,NATURAL) (1 . ,NATURAL) (2 . ,NATURAL)
                            (3 . ,NATURAL) (4 . ,NATURAL) (5 . ,NATURAL)
                            (6 . ,NATURAL))  e1   e1
                  \cadenzaOff
                }
                \addlyrics { \set stanza = \markup{ \normal-text "per ♮:" } la }                \addlyrics { \set stanza = \markup{ \normal-text "per ɴ:" } \skip 1 mi }
              }
            >>
            \layout {
              \enablePolymeter
%             #(layout-set-staff-size 15)

              \context {
                \Score
                \override SpacingSpanner.common-shortest-duration = #(ly:make-moment 1)
                \override LyricText.font-size = #'-1.0
                \override Accidental.stencil =
                #(lambda (grob)
                (let ((alteration (ly:pitch-alteration
                                   (ly:event-property
                                    (ly:grob-property (ly:grob-property grob 'cause) 'cause)
                                    'pitch))))
                 (cond ((= 1/4 alteration) (grob-interpret-markup grob (markup diesis)))
                       ((= 1/2 alteration) (grob-interpret-markup grob (markup diesisCromatico)))
                       (else (ly:accidental-interface::print grob)))))
              }
              \context {
                \Voice
                \consists Horizontal_bracket_engraver
                \override HorizontalBracket.direction = #UP
              }
            }
          }

        }

      }
      \hspace #3
      \center-column {
        \line {
          \left-align { \concat {  \normal-text "Ffaut" }  }
        }
        \vspace #0.2
        \line {

          \score {
            \new ChoirStaff
            <<
              \new Staff \with { instrumentName = ""}
              {
                \new Voice \with { \remove "Note_heads_engraver"
                                    \consists "Completion_heads_engraver"
                                    \remove "Rest_engraver"
                                    \consists "Completion_rest_engraver"}
                {
                  \sectionLabel \markup { \ellipse \fontsize #-8 "6" }
                  \accidentalStyle Score.forget
%                 \override Accidental.after-line-breaking = #'()
                  \override Rest.style = #'default
                  \override NoteHead.style = #'baroque
                  \cadenzaOn
                  \override Staff.TimeSignature.stencil = ##f

                  \clef "bass"
                  \key c #`((0 . ,NATURAL) (1 . ,NATURAL) (2 . ,NATURAL)
                            (3 . ,NATURAL) (4 . ,NATURAL) (5 . ,NATURAL)
                            (6 . ,NATURAL))  f1   f1
                  \cadenzaOff
                }
                \addlyrics { \set stanza = \markup{ \normal-text "per ɴ:" } fa }                \addlyrics { \set stanza = \markup{ \normal-text "per ♭:" } \skip 1 ut }
              }
            >>
            \layout {
              \enablePolymeter
%             #(layout-set-staff-size 15)

              \context {
                \Score
                \override SpacingSpanner.common-shortest-duration = #(ly:make-moment 1)
                \override LyricText.font-size = #'-1.0
                \override Accidental.stencil =
                #(lambda (grob)
                (let ((alteration (ly:pitch-alteration
                                   (ly:event-property
                                    (ly:grob-property (ly:grob-property grob 'cause) 'cause)
                                    'pitch))))
                 (cond ((= 1/4 alteration) (grob-interpret-markup grob (markup diesis)))
                       ((= 1/2 alteration) (grob-interpret-markup grob (markup diesisCromatico)))
                       (else (ly:accidental-interface::print grob)))))
              }
              \context {
                \Voice
                \consists Horizontal_bracket_engraver
                \override HorizontalBracket.direction = #UP
              }
            }
          }

        }

      }
      \hspace #3
      \center-column {
        \line {
          \left-align { \concat {  \normal-text "Gsolreut" }  }
        }
        \vspace #0.2
        \line {

          \score {
            \new ChoirStaff
            <<
              \new Staff \with { instrumentName = ""}
              {
                \new Voice \with { \remove "Note_heads_engraver"
                                    \consists "Completion_heads_engraver"
                                    \remove "Rest_engraver"
                                    \consists "Completion_rest_engraver"}
                {
                  \sectionLabel \markup { \ellipse \fontsize #-8 "7" }
                  \accidentalStyle Score.forget
%                 \override Accidental.after-line-breaking = #'()
                  \override Rest.style = #'default
                  \override NoteHead.style = #'baroque
                  \cadenzaOn
                  \override Staff.TimeSignature.stencil = ##f

                  \clef "bass"
                  \key c #`((0 . ,NATURAL) (1 . ,NATURAL) (2 . ,NATURAL)
                            (3 . ,NATURAL) (4 . ,NATURAL) (5 . ,NATURAL)
                            (6 . ,NATURAL))  g1   g1   g1
                  \cadenzaOff
                }
                \addlyrics { \set stanza = \markup{ \normal-text "per ɴ:" } sol }                \addlyrics { \set stanza = \markup{ \normal-text "per ♭:" } \skip 1 re }                \addlyrics { \set stanza = \markup{ \normal-text "per ♮:" } \skip 1 \skip 1 ut }
              }
            >>
            \layout {
              \enablePolymeter
%             #(layout-set-staff-size 15)

              \context {
                \Score
                \override SpacingSpanner.common-shortest-duration = #(ly:make-moment 1)
                \override LyricText.font-size = #'-1.0
                \override Accidental.stencil =
                #(lambda (grob)
                (let ((alteration (ly:pitch-alteration
                                   (ly:event-property
                                    (ly:grob-property (ly:grob-property grob 'cause) 'cause)
                                    'pitch))))
                 (cond ((= 1/4 alteration) (grob-interpret-markup grob (markup diesis)))
                       ((= 1/2 alteration) (grob-interpret-markup grob (markup diesisCromatico)))
                       (else (ly:accidental-interface::print grob)))))
              }
              \context {
                \Voice
                \consists Horizontal_bracket_engraver
                \override HorizontalBracket.direction = #UP
              }
            }
          }

        }

      }
      \hspace #3

    }
  }
}