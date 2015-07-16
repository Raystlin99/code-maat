;;; Copyright (C) 2015 Adam Tornhill
;;;
;;; Distributed under the GNU General Public License v3.0,
;;; see http://www.gnu.org/licenses/gpl.html

(ns code-maat.parsers.tfs
  (:require [instaparse.core :as insta]
            [code-maat.parsers.time-parser :as tp]
            [code-maat.parsers.hiccup-based-parser :as hbp]
            [clojure.string :as s]))

;;; NOTE:
;;; ====
;;; This is a quick and dirty parser for TFS. 
;;; The grammar is pretty unoptimized and we just read out 
;;; a minimum of information (we could mine more).
;;; Most importantly, we don't get any churn metrics so you cannot 
;;; run the churn analyses. We also don't parse the date field so 
;;; you cannot do things like group by date or run a code age analysis.
(def ^:const tfs-grammar
  "
    entry           = changeset <nl> user <nl> <checked-in-by*> date <nl> <nl> <comment> <nl> items <nl> <policy-warnings*>
    <changeset>     = <'Changeset: '> #'\\d+'
    <user>          = <'User: '> #'.+'
    checked-in-by   = #'Checked in by: .+' nl
    policy-warnings = 'Policy Warnings:' nl (ws+ #'.+' nl)* nl
    <date>          = <'Date: '> #'.+'
    comment         = 'Comment:' nl comment-line+
    comment-line    = ws+ #'.+' nl
    items           = <'Items:'> <nl> item*
    item            = <ws+> actions <ws+> file <nl>
    actions         = <'merge, '?> action
    <action>        = 'add' | 'edit' | 'delete' | 'merge' | 'branch'
    <file>          =  #'.+'
    ws              =  #'\\s'
    tab             =  #'\\t'
    nl              =  '\\n'")

;;; TODO: let's hardcode the date for now. We won't be able to run 
;;; all analyses, but we can run the most common ones.

(def as-common-time-format (tp/time-string-converter-from "YYYY-MM-dd"))

(defn- modification-of-interest?
  "Received input for one file like:
   '[:item [:actions edit] the/file/name.cs]'"
  [row]
  (-> row
      (get-in [1 1])
      #{"add" "edit" "merge"}))

(defn- adapt-to-shared-format
  [name]
  [:file name])

(defn- file-name
  [row]
  (-> row
      (get-in [2])
      s/trim
      adapt-to-shared-format))

(defn- modified-files
  "Drop everything that isn't add or edit.
   Note that we may have multiple TFS actions like 'merge, edit'."
  [files-with-action]
  (->> files-with-action
      rest ; strip the leading :items
      (filter modification-of-interest?)
      (map file-name)))

(def positional-extractors
  "Specify a set of functions to extract the parsed values."
  {:rev #(get-in % [1])
   :date (fn [line] (as-common-time-format "2015-07-01")) ; TODO: Correct this!
   :author #(get-in % [2])
   :changes #(modified-files (get-in % [4]))
   :message (fn [_] "-")
   :start-action rest ; skip the leading '---' token
   :separator (fn [line] (re-matches #"^\-+$" line))
  })

(defn parse-log
  "Transforms the given input git log into an
   Incanter dataset suitable for the analysis modules." 
  [input-file-name options]
  (hbp/parse-log input-file-name
                 options
                 tfs-grammar
                 positional-extractors))

(defn parse-read-log
  [input-text options]
  (hbp/parse-read-log input-text
                      options
                      tfs-grammar
                      positional-extractors))
