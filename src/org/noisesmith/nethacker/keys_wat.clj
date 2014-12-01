(ns org.noisesmith.nethacker.keys-wat
  (:require [clojure.string :as string])
  (:import (java.io StringReader PushbackReader FileReader)))
;;; Known to work with version 1:0.9.5-3 of nethack.el as available for debian
;;; systems.
;;; the fundamental issue here is that various parts of the nethack UI
;;; will prompt for input, but the way the input translation is done, we
;;; need to translate from the letter hinted by nethack-lisp to the command
;;; string that we need to send to activate that option.

;;; this is weirder than you may think
;;; eg. in character creation, when picking a class, it indicates that one
;;; would select "b" to create a barbarian. This has to be translated into
;;; the string "gosouthwest" in order to send it to the nethack-lisp process,
;;; because the character b means gosouthwest in the main UI.
;;; Kind of like a bizarro inside-out version of a key binding.

(defn extract-last-noncoll
  [f]
  (if (coll? f)
    (recur (last f))
    f))

(defn find-bindings
  "We use clojure \"read\" to scrape some data from an elisp file. This is a
  hack.
  We make some big assumptions about the structure of the elisp file, and
  extract key bindings and command names from the repetitive forms. This could
  easily break if the elisp code was refactored, but does the job nicely for
  now."
  [mode forms]
  (let [get-shortcut #(nth % 2)
        get-command (fn [command-sexp]
                      (let [command-symbol (extract-last-noncoll command-sexp)
                            command (.replace (name command-symbol)
                                              "nethack-command-" "")]
                        command))]
    (-> forms
        (->>
         (filter #(= (take 2 %) ['defvar mode])))
        first
        (nth 2)
        (->> (filter #(and (coll? %)
                           (= (first %) 'define-key)))
             (map (juxt get-shortcut get-command))))))

(defn get-forms
  "Read a bunch of elisp forms with the Clojure reader. Don't be surprised if
  this totally breaks with future versions of nethack.el."
  [pbr]
  ((fn forms []
     (when-let [form (try (read pbr)
                          (catch Exception _ nil))]
       (cons form (forms))))))

(defn get-keys
  "extracts each keybinding and the command it runs in nethack-lisp"
  [source-file]
  (let [source (-> source-file
                   slurp
                   (string/replace "\\C" "C")
                   (string/replace "\\e" "#")
                   (string/replace "\\M" "M")
                   StringReader.
                   PushbackReader.)
        forms (get-forms source)
        map-mode (into {} (find-bindings 'nh-map-mode-map forms))
        menu-mode (into {} (find-bindings 'nh-menu-mode-map forms))]
    {:map map-mode
     :menu menu-mode}))

(defn get-commands
  "extracts the command name and the string it sends for each defined command
  that nethack-lisp understands"
  [source-file]
  (let [source (-> source-file
                   FileReader.
                   PushbackReader.)
        forms (get-forms source)
        command-name #(name (nth % 1))
        command-string extract-last-noncoll]
    (->> forms
         (filter #(= (first %) 'defun-nethack-command))
         (map (juxt command-name command-string))
         (into {}))))

(defn build-commands
  "builds a mapping from shortcut keys to commands understood by nethack-lisp"
  [& [key-file command-file]]
  (let [default-key "/usr/share/emacs/site-lisp/nethack/nethack-keys.el"
        default-command "/usr/share/emacs/site-lisp/nethack/nethack-cmd.el"
        keys (apply merge ((juxt :menu :map)
                           (get-keys (or key-file default-key))))
        commands (get-commands
                  (or command-file default-command))]
    (reduce (fn [bindings [shortcut command]]
              (assoc bindings shortcut (get commands command)))
            {}
            keys)))
