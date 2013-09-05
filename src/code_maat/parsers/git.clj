;;; Copyright (C) 2013 Adam Tornhill
;;;
;;; Distributed under the GNU General Public License v3.0,
;;; see http://www.gnu.org/licenses/gpl.html

(ns code-maat.parsers.git
  (:require [instaparse.core :as insta]
            [code-maat.parsers.hiccup-based-parser :as hbp]
            [incanter.core :as incanter]))

;;; This module is responsible for parsing a git log file.
;;;
;;; Input: a log file generated with the following command:
;;;         
;;;    git log --pretty=format:'[%h] %an %ad %s' --name-status --date=short 
;;;
;;; Ouput: An incanter dataset with the following columns:
;;;   :entity :date :author :rev
;;; where
;;;  :entity -> the changed entity as a string
;;;  :date -> commit date as a string
;;;  :author -> as a string
;;;  :rev -> the hash used by git to identify the commit

(def ^:const git-grammar
  "Here's the instaparse grammar for a git log-file.
   In the current version we only extract basic info on
   authors and file modification patterns.
   As we add more analysis options (e.g. churn), it gets
   interesting to enable more parse output."
   "
    <S>       =   entries
    <entries> =  (entry <nl*>)* | entry
    entry     =  rev <ws> author <ws> date <ws> <message> <nl> changes
    rev       =  <'['> #'[\\da-f]+' <']'>
    author    =  #'.+(?=\\s\\d{4}-)' (* match until the date field *)
    date      =  #'\\d{4}-\\d{2}-\\d{2}'
    message   =  #'.+'?
    changes   =  change*
    <change>  =  <action> <tab> file <nl>
    action    =  #'.' (* added, modified, etc - single character *) 
    file      =  #'.+'
    ws        =  #'\\s'
    tab       =  #'\\t'
    nl        =  '\\n'
    hash      =  #'[\\da-f]+'")

(defn parse-log
  "Transforms the given input git log into an
   Incanter dataset suitable for the analysis modules." 
  [input parse-options]
  (hbp/parse-log input git-grammar parse-options))
