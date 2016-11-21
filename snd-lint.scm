;;; Snd extensions for lint

(require lint.scm)
;;; *lint* is the lint environment, so everything in lint.scm is accessible here

;;; --------------------------------
;;; this sends lint's output to the Snd repl's widget
(define (snd-lint file)
  (lint file (openlet 
	      (inlet :name "lint-output-port"
		     :format            (lambda (p str . args) (snd-print (apply format #f str args)))
		     :write             (lambda (obj p)	       (snd-print (object->string obj #t)))
		     :display           (lambda (obj p)	       (snd-print (object->string obj #f)))
		     :write-string      (lambda (str p)        (snd-print str))
		     :write-char        (lambda (ch p)	       (snd-print (string ch)))
		     :newline           (lambda (p)	       (snd-print (string #\newline)))
		     :close-output-port (lambda (p) #f)
		     :flush-output-port (lambda (p) #f)))))


;;; ---------------- deprecated funcs ----------------
;;; Snd deprecated funcs, to be reported by lint

(let ((deprecated-ops '((data-format . sample-type)
			(mus-sound-frames . mus-sound-framples)
			(mus-sound-data-format . mus-sound-sample-type)
			(mus-data-format-name . mus-sample-type-name)
			(mus-data-format->string . mus-sample-type->string)
			(default-output-data-format . default-output-sample-type)
			(channel->vct . channel->float-vector)
			(vct->channel . float-vector->channel)
			(region->vct . region->float-vector)
			(mix->vct . mix->float-vector)
			(transform->vct . transform->float-vector)
			(make-vct . make-float-vector)
			(vct-add! . float-vector-add!)
			(vct-subtract! . float-vector-subtract!)
			(vct-copy . copy)
			(vct-length . length)
			(vct-multiply! . float-vector-multiply!)
			(vct-offset! . float-vector-offset!)
			(vct-ref . float-vector-ref)
			(vct-scale! . float-vector-scale!)
			(vct-abs! . float-vector-abs!)
			(vct-fill! . fill!)
			(vct-set! . float-vector-set!)
			(vct-peak . float-vector-peak)
			(vct-peak-and-location . float-vector-peak-and-location)
			(vct-equal? . equal?)
			(vct? . float-vector?)
			(list->vct . list->float-vector)
			(vct->list . float-vector->list)
			(vector->vct . vector->float-vector)
			(vct->vector . float-vector->vector)
			(vct-move! . float-vector-move!)
			(vct-subseq . float-vector-subseq)
			(vct-reverse! . reverse!)
			(vct->string . float-vector->string)
			(vct* . float-vector*)
			(vct+ . float-vector+))))

  (define (snd-lint-deprecate caller head form env)
    ((*lint* 'lint-format) "~A is deprecated; use ~A" caller head (cond ((assq head deprecated-ops) => cdr))))

  (for-each (lambda (op)
	      (hash-table-set! (*lint* 'special-case-functions) (car op) snd-lint-deprecate))
	    deprecated-ops))


;;; ---------------- snd-display ----------------
;;; check snd-display using format's lint code

(hash-table-set! (*lint* 'special-case-functions) 'snd-display
		 (hash-table-ref (*lint* 'special-case-functions) 'format))


;;; ---------------- defgenerator ----------------
;;; a lint walker for defgenerator (it defines various functions in the current environment)

(let ()
  (define (get-generator caller form env)
    (with-let (sublet *lint* :caller caller :form form :env env)
      (when (pair? (cdr form))
	(let ((name (symbol->string ((if (pair? (cadr form)) caadr cadr) form))))
	  
	  (if (and (pair? (cadr form))
		   (pair? (cdadr form)))
	      (lint-walk caller (cdadr form) env))
	  
	  (let ((gen? (symbol name "?"))
		(gen-make (symbol "make-" name)))
	    (list (make-fvar :name gen?
			     :ftype 'define
			     :decl (dummy-func 'define `(define (,gen? x) (let? x)) '(define (_ x) #f))
			     :initial-value `(define (,gen? x) (let? x))
			     :arglist (list 'x)
			     :env env)
		  (make-fvar :name gen-make
			     :ftype 'define*
			     :decl (dummy-func 'define* `(define* (,gen-make :rest x :allow-other-keys) (apply inlet x)) '(define (_ . x) #f))
			     :initial-value `(define* (,gen-make :rest x :allow-other-keys) (apply inlet x))
			     :arglist (list :rest 'x :allow-other-keys)
			     :env env)))))))
  
  (hash-table-set! (*lint* 'walker-functions) 'defgenerator
		   (lambda (caller form env)
		     (append (get-generator caller form env) env))))

	
;;; ---------------- no side effect Snd functions ----------------
;;; Snd functions that don't affect anything outside or mess with their arguments

(let ((h (*lint* 'no-side-effect-functions)))
  (for-each
   (lambda (s)
     (hash-table-set! h s #t))
   '(all-chans all-pass-bank? all-pass? apropos array-interp ask-about-unsaved-edits ask-before-overwrite
     asymmetric-fm? axis-color axis-info axis-label-font axis-numbers-font basic-color
     beats-per-measure beats-per-minute bes-i0 bes-i1 bes-in bes-j0 bes-j1 bes-jn bes-k0 bes-k1 bes-kn bes-y0 bes-y1 bes-yn
     bold-peaks-font channel-amp-envs channel-data channel-properties channel-property channel-style
     channel-sync channel-widgets channels channels-equal? channels=? chans clipping
     clm-default-frequency clm-table-size color->list color-cutoff color-inverted color-scale
     color? colormap colormap-name colormap-ref colormap-size colormap? comb-bank?
     comb? combined-data-color comment contrast-control contrast-control-amp contrast-control-bounds contrast-control?
     contrast-enhancement convolve? count-matches current-font
     current-time cursor cursor-color cursor-location-offset cursor-position cursor-size cursor-style
     cursor-update-interval dac-combines-channels dac-size data-color data-location data-size db->linear
     default-output-chans default-output-header-type default-output-sample-type default-output-srate degrees->radians delay?
     dialog-widgets disk-kspace dot-product dot-size
     edit-fragment edit-fragment-type-name edit-list->function edit-properties edit-property
     edit-tree edits edot-product env? enved-base enved-clip?
     enved-envelope enved-filter enved-filter-order enved-in-dB enved-power enved-style enved-target
     enved-wave? enved-waveform-color envelope-interp eps-bottom-margin eps-file eps-left-margin
     eps-size erf erfc even-multiple even-weight expand-control expand-control-bounds
     expand-control-hop expand-control-jitter expand-control-length expand-control-ramp expand-control? feql ffeql
     fffneq ffneq fft-log-frequency fft-log-magnitude fft-window fft-window-alpha
     fft-window-beta fft-with-phases file->frample? file->sample? file-name file-write-date filter-control-coeffs filter-control-envelope
     filter-control-in-dB filter-control-in-hz filter-control-order filter-control-waveform-color filter-control? filter? filtered-comb-bank?
     filtered-comb? find-dialog find-mark find-sound fir-filter? firmant? float-vector-equal? float-vector-max float-vector-min float-vector-peak
     float-vector-peak-and-location fneq foreground-color formant-bank? formant? frample
     framples ftell fveql get-internal-real-time getcwd getpid granulate? graph-color graph-cursor
     graph-style graphs-horizontal grid-density gsl-ellipj gsl-ellipk gsl-roots header-type html-dir html-program hz->radians
     identity iir-filter? initial-beg initial-dur 
     integer->mark integer->mix integer->region integer->sound integer->transform just-sounds key-binding
     ladspa-dir left-sample lgamma linear->db lisp-graph-style lisp-graph?
     listener-color listener-colorized listener-font listener-prompt listener-selection listener-text-color little-endian?
     localtime locsig-ref locsig-reverb-ref locsig-type locsig? log-freq-start main-menu main-widgets 

     make-env make-pulsed-env make-one-pole make-fir-coeffs make-formant make-all-pass-bank make-iir-filter make-filter 
     make-comb make-polywave make-bezier make-delay make-nrxycos make-moving-norm make-nrxysin make-firmant make-cairo 
     make-sawtooth-wave make-color make-graph-data make-oscil make-oscil-bank make-two-zero make-fft-window make-moving-max 
     make-filtered-comb-bank make-filtered-comb make-nsin make-rand-interp make-one-pole-all-pass make-rand make-formant-bank 
     make-all-pass make-table-lookup make-one-zero make-notch make-square-wave make-moving-average make-polyshape
     make-triangle-wave make-comb-bank make-ncos make-rxyk!sin make-fir-filter make-two-pole make-asymmetric-fm 
     make-rxyk!cos make-pulse-train 

     mark->integer mark-color mark-home mark-hook mark-name mark-properties
     mark-property mark-sample mark-sync mark-sync-max mark-tag-height mark-tag-width mark?
     marks max-regions max-transform-peaks maxamp maxamp-position menu-widgets min-dB mix->integer mix-color mix-dialog-mix mix-drag-hook mix-home
     mix-length mix-name mix-properties mix-property mix-sampler? mix-sync mix-sync-max mix-tag-height mix-tag-width mix-tag-y mix-waveform-height mix? mixes
     move-sound? moving-average? moving-max? moving-norm? mus-alsa-buffer-size
     mus-alsa-buffers mus-alsa-capture-device mus-alsa-device mus-alsa-playback-device mus-alsa-squelch-warning mus-array-print-length mus-bytes-per-sample
     mus-channel mus-channels mus-chebyshev-t-sum mus-chebyshev-tu-sum mus-chebyshev-u-sum mus-clipping mus-data
     mus-describe mus-error-type->string mus-expand-filename mus-file-buffer-size mus-file-clipping mus-file-name
     mus-float-equal-fudge-factor mus-frequency mus-generator? mus-header-raw-defaults mus-header-type->string mus-header-type-name mus-header-writable
     mus-hop mus-increment mus-input? mus-interp-type mus-interpolate mus-length mus-location mus-max-malloc mus-max-table-size mus-name mus-offset mus-order
     mus-output? mus-phase mus-ramp mus-sample-type->string mus-sample-type-name mus-scaler
     mus-sound-chans mus-sound-comment mus-sound-data-location mus-sound-datum-size mus-sound-duration mus-sound-frames
     mus-sound-framples mus-sound-header-type mus-sound-length mus-sound-loop-info mus-sound-mark-info mus-sound-maxamp mus-sound-maxamp-exists?
     mus-sound-path mus-sound-sample-type mus-sound-samples mus-sound-srate
     mus-sound-type-specifier mus-sound-write-date mus-srate mus-type mus-width mus-xcoeff mus-xcoeffs
     mus-ycoeff mus-ycoeffs mus_header_t? ncos? notch? nrxycos? nrxysin?
     nsin? odd-multiple odd-weight one-pole-all-pass? one-pole? one-zero? oscil-bank?
     oscil? partials->polynomial partials->wave pausing peak-env-dir peaks-font
     phase-partials->wave phase-vocoder-amp-increments phase-vocoder-amps phase-vocoder-freqs phase-vocoder-phase-increments phase-vocoder-phases phase-vocoder?
     phases-get-peak play-arrow-size player-home player? players playing polyshape?
     polywave? position->x position->y position-color pulse-train? pulsed-env? radians->degrees radians->hz rand-interp?
     rand? readin? region->integer region-chans region-framples region-graph-style region-home
     region-maxamp region-maxamp-position region-position region-sample region-sampler? region-srate region?
     regions reverb-control-decay reverb-control-feedback reverb-control-length reverb-control-length-bounds reverb-control-lowpass reverb-control-scale
     reverb-control-scale-bounds reverb-control? right-sample rxyk!cos? rxyk!sin? sample sample-type
     sampler-at-end? sampler-home sampler-position sampler? samples samples->seconds sash-color
     sawtooth-wave? script-arg script-args search-procedure seconds->samples selected-channel selected-data-color
     selected-graph-color selected-sound selection selection-chans selection-creates-region selection-framples selection-maxamp
     selection-maxamp-position selection-member? selection-position selection-srate selection? short-file-name
     sinc-width singer-filter singer-nose-filter snd->sample? snd-gcs snd-global-state snd-help snd-tempnam
     snd-url snd-urls snd-version sound->integer sound-file-extensions sound-file?
     sound-files-in-directory sound-loop-info sound-properties sound-property sound-widgets sound? soundfont-info
     sounds spectro-hop spectro-x-angle spectro-x-scale spectro-y-angle spectro-y-scale spectro-z-angle
     spectro-z-scale spectrum-end spectrum-start speed-control speed-control-bounds speed-control-style
     speed-control-tones square-wave? srate src? ssb-am? strftime sync-max sync-style syncd-marks table-lookup? tap? temp-dir
     text-focus-color time-graph-style time-graph-type time-graph? tiny-font tmpnam tracking-cursor-style
     transform->integer transform-graph-style transform-graph-type transform-graph?
     transform-normalization transform-sample transform-size transform-type transform? triangle-wave? two-pole?
     two-zero? variable-graph? wave-train? wavelet-type
     wavo-hop wavo-trace widget-position widget-size widget-text window-height window-width
     window-x window-y with-background-processes with-file-monitor with-gl with-inset-graph with-interrupts
     with-menu-icons with-mix-tags with-pointer-focus with-relative-panes with-smpte-label with-toolbar with-tooltips
     with-tracking-cursor with-verbose-cursor x->position x-axis-label x-axis-style x-bounds x-position-slider
     x-zoom-slider y->position y-axis-label y-bounds y-position-slider y-zoom-slider zoom-color
     zoom-focus-style zoom-one-pixel)))


;;; ---------------- Snd makers ----------------
(let ((h (*lint* 'makers)))
  (for-each
   (lambda (s)
     (hash-table-set! h s #t))
   '(make-env make-pulsed-env make-one-pole make-fir-coeffs make-convolve make-wave-train make-formant make-all-pass-bank 
     make-iir-filter make-filter make-comb make-sample->file make-polywave make-bezier make-delay make-nrxycos make-moving-norm 
     make-nrxysin make-firmant make-cairo make-sawtooth-wave make-color make-player make-graph-data make-oscil make-oscil-bank 
     make-two-zero make-fft-window make-moving-max make-filtered-comb-bank make-filtered-comb make-nsin make-rand-interp 
     make-one-pole-all-pass make-rand make-formant-bank make-readin make-all-pass make-phase-vocoder make-table-lookup 
     make-one-zero make-notch make-square-wave make-file->frample make-moving-average make-granulate make-polyshape 
     make-locsig make-triangle-wave make-mix-sampler make-move-sound make-comb-bank make-ncos make-rxyk!sin 
     make-variable-graph make-fir-filter make-file->sample make-ssb-am make-two-pole make-region-sampler 
     make-frample->file make-asymmetric-fm make-sampler make-region make-snd->sample make-src make-rxyk!cos make-pulse-train)))


;;; ---------------- Snd booleans ----------------
;;; add Snd/clm type checkers to lint's table (lint assumes that these take one argument)

(for-each (lambda (tchk)
	    (hash-table-set! (*lint* 'booleans) tchk #t))
	  '(all-pass? all-pass-bank? asymmetric-fm? comb? comb-bank? convolve? delay? env? file->sample? filter? filtered-comb?
	    filtered-comb-bank? fir-filter? firmant? formant-bank? formant? granulate? iir-filter? locsig? move-sound? moving-average?
	    moving-max? moving-norm? mus-generator? mus-input? mus-output? ncos? notch? nrxycos? nrxysin? nsin? one-pole? one-pole-all-pass? 
	    one-zero? oscil? oscil-bank? phase-vocoder? polyshape? polywave? pulse-train? pulsed-env? rand-interp? rand? readin?
	    rxyk!cos? rxyk!sin? sample->file? sawtooth-wave? square-wave? src? ssb-am? table-lookup? tap? triangle-wave? two-pole?
	    two-zero? wave-train? file->frample? frample->file?

	    mark? mix? mix-sampler? region?))
