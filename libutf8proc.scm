;;; utf8proc.scm
;;;
;;; tie the utf8proc library into the *utf8proc* environment

(require cload.scm)
(provide 'libutf8proc.scm)

;; if loading from a different directory, pass that info to C
(let ((directory (let ((current-file (port-filename)))
		   (and (memv (current-file 0) '(#\/ #\~))
			(substring current-file 0 (- (length current-file) 9))))))
  (when (and directory (not (member directory *load-path*)))
    (set! *load-path* (cons directory *load-path*)))
  (with-let (rootlet)
    (require cload.scm))
  (when (and directory (not (string-position directory *cload-cflags*)))
    (set! *cload-cflags* (string-append "-I" directory " " *cload-cflags*))))


(if (not (defined? '*libutf8proc*))
    (define *libutf8proc*
      (with-let (unlet)
	
	(set! *libraries* (cons (cons "libutf8proc.scm" (curlet)) *libraries*))

	(c-define 
	 '((C-macro (int (UTF8PROC_VERSION_MAJOR UTF8PROC_VERSION_MINOR UTF8PROC_VERSION_PATCH)))

	   (int (UTF8PROC_NULLTERM UTF8PROC_STABLE UTF8PROC_COMPAT UTF8PROC_COMPOSE UTF8PROC_DECOMPOSE
		 UTF8PROC_IGNORE UTF8PROC_REJECTNA UTF8PROC_NLF2LS UTF8PROC_NLF2PS UTF8PROC_NLF2LF   
		 UTF8PROC_STRIPCC UTF8PROC_CASEFOLD UTF8PROC_CHARBOUND UTF8PROC_LUMP UTF8PROC_STRIPMARK))

	   (C-macro (int (UTF8PROC_ERROR_NOMEM UTF8PROC_ERROR_OVERFLOW UTF8PROC_ERROR_INVALIDUTF8 UTF8PROC_ERROR_NOTASSIGNED UTF8PROC_ERROR_INVALIDOPTS)))

	   (int (UTF8PROC_CATEGORY_CN UTF8PROC_CATEGORY_LU UTF8PROC_CATEGORY_LL UTF8PROC_CATEGORY_LT UTF8PROC_CATEGORY_LM
		 UTF8PROC_CATEGORY_LO UTF8PROC_CATEGORY_MN UTF8PROC_CATEGORY_MC UTF8PROC_CATEGORY_ME UTF8PROC_CATEGORY_ND
                 UTF8PROC_CATEGORY_NL UTF8PROC_CATEGORY_NO UTF8PROC_CATEGORY_PC UTF8PROC_CATEGORY_PD UTF8PROC_CATEGORY_PS
                 UTF8PROC_CATEGORY_PE UTF8PROC_CATEGORY_PI UTF8PROC_CATEGORY_PF UTF8PROC_CATEGORY_PO UTF8PROC_CATEGORY_SM
                 UTF8PROC_CATEGORY_SC UTF8PROC_CATEGORY_SK UTF8PROC_CATEGORY_SO UTF8PROC_CATEGORY_ZS UTF8PROC_CATEGORY_ZL
                 UTF8PROC_CATEGORY_ZP UTF8PROC_CATEGORY_CC UTF8PROC_CATEGORY_CF UTF8PROC_CATEGORY_CS UTF8PROC_CATEGORY_CO))

	   (int (UTF8PROC_BIDI_CLASS_L UTF8PROC_BIDI_CLASS_LRE UTF8PROC_BIDI_CLASS_LRO UTF8PROC_BIDI_CLASS_R UTF8PROC_BIDI_CLASS_AL
                 UTF8PROC_BIDI_CLASS_RLE UTF8PROC_BIDI_CLASS_RLO UTF8PROC_BIDI_CLASS_PDF UTF8PROC_BIDI_CLASS_EN UTF8PROC_BIDI_CLASS_ES
                 UTF8PROC_BIDI_CLASS_ET UTF8PROC_BIDI_CLASS_AN UTF8PROC_BIDI_CLASS_CS UTF8PROC_BIDI_CLASS_NSM UTF8PROC_BIDI_CLASS_BN
                 UTF8PROC_BIDI_CLASS_B UTF8PROC_BIDI_CLASS_S UTF8PROC_BIDI_CLASS_WS UTF8PROC_BIDI_CLASS_ON UTF8PROC_BIDI_CLASS_LRI
                 UTF8PROC_BIDI_CLASS_RLI UTF8PROC_BIDI_CLASS_FSI UTF8PROC_BIDI_CLASS_PDI))

	   (int (UTF8PROC_DECOMP_TYPE_FONT UTF8PROC_DECOMP_TYPE_NOBREAK UTF8PROC_DECOMP_TYPE_INITIAL UTF8PROC_DECOMP_TYPE_MEDIAL
                 UTF8PROC_DECOMP_TYPE_FINAL UTF8PROC_DECOMP_TYPE_ISOLATED UTF8PROC_DECOMP_TYPE_CIRCLE UTF8PROC_DECOMP_TYPE_SUPER
                 UTF8PROC_DECOMP_TYPE_SUB UTF8PROC_DECOMP_TYPE_VERTICAL UTF8PROC_DECOMP_TYPE_WIDE UTF8PROC_DECOMP_TYPE_NARROW
                 UTF8PROC_DECOMP_TYPE_SMALL UTF8PROC_DECOMP_TYPE_SQUARE UTF8PROC_DECOMP_TYPE_FRACTION UTF8PROC_DECOMP_TYPE_COMPAT))

	   (int (UTF8PROC_BOUNDCLASS_START UTF8PROC_BOUNDCLASS_OTHER UTF8PROC_BOUNDCLASS_CR UTF8PROC_BOUNDCLASS_LF
                 UTF8PROC_BOUNDCLASS_CONTROL UTF8PROC_BOUNDCLASS_EXTEND UTF8PROC_BOUNDCLASS_L UTF8PROC_BOUNDCLASS_V
                 UTF8PROC_BOUNDCLASS_T UTF8PROC_BOUNDCLASS_LV UTF8PROC_BOUNDCLASS_LVT UTF8PROC_BOUNDCLASS_REGIONAL_INDICATOR
                 UTF8PROC_BOUNDCLASS_SPACINGMARK))

	   ;; in version 2: 
	   ;;    UTF8PROC_BOUNDCLASS_PREPEND UTF8PROC_BOUNDCLASS_ZWJ UTF8PROC_BOUNDCLASS_E_BASE
	   ;;    UTF8PROC_BOUNDCLASS_E_MODIFIER UTF8PROC_BOUNDCLASS_GLUE_AFTER_ZWJ UTF8PROC_BOUNDCLASS_E_BASE_GAZ
	   ;;    utf8proc_int32_t utf8proc_totitle(utf8proc_int32_t c)
	   ;;    utf8proc_bool utf8proc_grapheme_break_stateful(utf8proc_int32_t codepoint1, utf8proc_int32_t codepoint2, utf8proc_int32_t *state)

	   (char* utf8proc_version (void))
	   (char* utf8proc_errmsg (int))
	   (int utf8proc_tolower ((utf8proc_int32_t int)))
	   (int utf8proc_toupper ((utf8proc_int32_t int)))
	   (int utf8proc_charwidth ((utf8proc_int32_t int)))
	   (int utf8proc_category ((utf8proc_int32_t int)))
	   (char* utf8proc_category_string ((utf8proc_int32_t int)))
	   (bool utf8proc_codepoint_valid ((utf8proc_int32_t int)))
	   (bool utf8proc_grapheme_break ((utf8proc_int32_t int) (utf8proc_int32_t int)))

	   (char* utf8proc_NFD (char*))
	   (char* utf8proc_NFC (char*))
	   (char* utf8proc_NFKD (char*))
	   (char* utf8proc_NFKC (char*))

	   (in-C "static s7_pointer g_utf8proc_iterate(s7_scheme *sc, s7_pointer args)
                  {
                    utf8proc_int32_t code_ref = 0;
                    int len, res;
                    char *str;
                    str = (char *)s7_string(s7_car(args));
                    len = s7_string_length(s7_car(args));
                    res = utf8proc_iterate(str, len, &code_ref);
                    return(s7_list(sc, 2, s7_make_integer(sc, code_ref), s7_make_integer(sc, res)));
                   }")
	   (C-function ("utf8proc_iterate" g_utf8proc_iterate "" 1))

	   (in-C "static s7_pointer g_utf8proc_encode_char(s7_scheme *sc, s7_pointer args)
                  {
                    ssize_t res;
                    utf8proc_uint8_t buf[8];
                    res = utf8proc_encode_char((utf8proc_int32_t)s7_integer(s7_car(args)), buf);
                    return(s7_list(sc, 2, s7_make_string_with_length(sc, buf, res), s7_make_integer(sc, res)));
                   }")
	   (C-function ("utf8proc_encode_char" g_utf8proc_encode_char "" 1))

	   (in-C "static s7_pointer g_utf8proc_reencode(s7_scheme *sc, s7_pointer args)
                  {
                    s7_pointer buffer, codepoints, options;
                    ssize_t res;
                    buffer = s7_car(args);
                    codepoints = s7_cadr(args);
                    options = s7_caddr(args);
                    res = utf8proc_reencode((utf8proc_int32_t *)s7_string(buffer), 
                                            (utf8proc_ssize_t)s7_integer(codepoints), 
                                            (utf8proc_option_t)s7_integer(options));
                    return(s7_make_integer(sc, res));
                   }")
	   (C-function ("utf8proc_reencode" g_utf8proc_reencode "" 1))
	   (in-C "static s7_pointer g_utf8proc_get_property(s7_scheme *sc, s7_pointer args)
                  {
	            const utf8proc_property_t *info;
                    info = utf8proc_get_property((utf8proc_int32_t)s7_integer(s7_car(args)));
                    return(s7_inlet(sc, s7_list(sc, 30,
                             s7_make_symbol(sc, \"category\"),           s7_make_integer(sc, info->category),
                             s7_make_symbol(sc, \"combining_class\"),    s7_make_integer(sc, info->combining_class),
                             s7_make_symbol(sc, \"bidi_class\"),         s7_make_integer(sc, info->bidi_class),
                             s7_make_symbol(sc, \"decomp_type\"),        s7_make_integer(sc, info->decomp_type),
                 #if (UTF8PROC_VERSION_MAJOR >= 2)
                             s7_make_symbol(sc, \"uppercase_seqindex\"), s7_make_integer(sc, info->uppercase_seqindex),
                             s7_make_symbol(sc, \"lowercase_seqindex\"), s7_make_integer(sc, info->lowercase_seqindex),
                             s7_make_symbol(sc, \"titlecase_seqindex\"), s7_make_integer(sc, info->titlecase_seqindex),
                             s7_make_symbol(sc, \"casefold_seqindex\"),  s7_make_integer(sc, info->casefold_seqindex),
                             s7_make_symbol(sc, \"comb_index\"),         s7_make_integer(sc, info->comb_index),
                 #else
                             s7_make_symbol(sc, \"uppercase_mapping\"),  s7_make_integer(sc, info->uppercase_mapping),
                             s7_make_symbol(sc, \"lowercase_mapping\"),  s7_make_integer(sc, info->lowercase_mapping),
                             s7_make_symbol(sc, \"titlecase_mapping\"),  s7_make_integer(sc, info->titlecase_mapping),
                             s7_make_symbol(sc, \"comb1st_index\"),      s7_make_integer(sc, info->comb1st_index),
                             s7_make_symbol(sc, \"comb2nd_index\"),      s7_make_integer(sc, info->comb2nd_index),
                 #endif
                             s7_make_symbol(sc, \"bidi_mirrored\"),      s7_make_integer(sc, info->bidi_mirrored),
                             s7_make_symbol(sc, \"comp_exclusion\"),     s7_make_integer(sc, info->comp_exclusion),
                             s7_make_symbol(sc, \"ignorable\"),          s7_make_integer(sc, info->ignorable),
                             s7_make_symbol(sc, \"control_boundary\"),   s7_make_integer(sc, info->control_boundary),
                             s7_make_symbol(sc, \"boundclass\"),         s7_make_integer(sc, info->boundclass),
                             s7_make_symbol(sc, \"charwidth\"),          s7_make_integer(sc, info->charwidth))));
                   }")
	   (C-function ("utf8proc_get_property" g_utf8proc_get_property "" 1))

	   (in-C "static s7_pointer g_utf8proc_decompose_char(s7_scheme *sc, s7_pointer args)
                  {
                    s7_pointer code, opt, str;
                    int last_boundclass;
                    utf8proc_ssize_t size;
                    utf8proc_int32_t *dst;
                    ssize_t res;
                    code = s7_car(args);
                    str = s7_cadr(args);
                    opt = s7_caddr(args);
                    dst = (utf8proc_int32_t *)s7_string(str);
                    size = (utf8proc_ssize_t)s7_string_length(str);
                    res = utf8proc_decompose_char((utf8proc_int32_t)s7_integer(code), dst, size, (utf8proc_option_t)s7_integer(opt), &last_boundclass);
                    return(s7_make_integer(sc, res));
                  }")
	   (C-function ("utf8proc_decompose_char" g_utf8proc_decompose_char "" 3))

	   (in-C "static s7_pointer g_utf8proc_map(s7_scheme *sc, s7_pointer args)
                  {
                    s7_pointer opt, str;
                    ssize_t res;
                    utf8proc_uint8_t *dst;
                    str = s7_car(args);
                    opt = s7_cadr(args);
                    res = utf8proc_map((utf8proc_uint8_t *)s7_string(str), s7_string_length(str), &dst, (utf8proc_option_t)s7_integer(opt));
                    if (res < 0) return(s7_make_integer(sc, res));
                    return(s7_make_string_with_length(sc, dst, res));
                  }")
	   (C-function ("utf8proc_map" g_utf8proc_map "" 2))

	   (in-C "static s7_pointer g_utf8proc_decompose(s7_scheme *sc, s7_pointer args)
                  {
                    s7_pointer opt, str;
                    int len;
                    ssize_t res;
                    utf8proc_int32_t *dst;
                    str = s7_car(args);
                    opt = s7_cadr(args);
                    len = s7_string_length(str);
                    dst = (utf8proc_int32_t *)malloc(len * 4);
                    res = utf8proc_decompose((const utf8proc_uint8_t *)s7_string(str), len, dst, len, (utf8proc_option_t)s7_integer(opt));
                    if (res < 0) return(s7_make_integer(sc, res));
                    return(s7_make_string_with_length(sc, (char *)dst, res));
                  }")
	   (C-function ("utf8proc_decompose" g_utf8proc_decompose "" 2))
	   )

	 "" "utf8proc.h" "" "-lutf8proc" "utf8proc_s7")
	(curlet))))

*libutf8proc*
