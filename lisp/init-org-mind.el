;;; lisp/init-org-mind.el -*- lexical-binding: t; -*-

;; This is an Emacs package that creates graphviz directed graphs from
;; the headings of an org file
(use-package org-mind-map
    :straight t
    :init
    (require 'ox-org)
    :ensure t
    ;; Uncomment the below if 'ensure-system-packages` is installed
    ;;:ensure-system-package (gvgen . graphviz)
    ;; :config
    :custom
    (org-mind-map-engine "dot")
    ;; (setq org-mind-map-engine "dot")       ; Default. Directed Graph
    ;; (setq org-mind-map-engine "neato")  ; Undirected Spring Graph
    ;; (setq org-mind-map-engine "twopi")  ; Radial Layout
    ;; (setq org-mind-map-engine "fdp")    ; Undirected Spring Force-Directed
    ;; (setq org-mind-map-engine "sfdp")   ; Multiscale version of fdp for the layout of large graphs
    ;; (setq org-mind-map-engine "twopi")  ; Radial layouts
    ;; (setq org-mind-map-engine "circo")  ; Circular Layout
    )

(provide 'init-org-mind)
;;; init-org-mind.el ends here
