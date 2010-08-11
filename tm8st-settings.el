("tm8st.el"
 (tm8st-all-settings
  virtual nil
  "this is a virtual package to build garaemon's emacs environment"
  (tm8st-anything-settings))
 
 (tm8st-anything-settings
  library
  (http://www.emacswiki.org/emacs/download/anything-gtags.el
   http://www.emacswiki.org/emacs/download/anything-etags.el
   http://www.emacswiki.org/emacs/download/anything-extension.el
   http://www.emacswiki.org/emacs/download/anything-kyr.el
   http://www.emacswiki.org/emacs/download/anything-c-moccur
   http://www.emacswiki.org/emacs/download/anything-project
   http://www.emacswiki.org/emacs/download/anything-howm)
  "settings for anything."
  nil
  (:byte-compile))
 )
