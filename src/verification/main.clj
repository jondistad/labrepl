(ns verification.main
  (:use [labrepl.lab :only (instructions)]
	net.cgrand.enlive-html
	[clojure.string :only (trim)])
  (:require [hiccup.core :as hc])
  (:import (java.io ByteArrayInputStream)))

(defn code-map* [lines]
  (let [has-result (re-find #"^->" (last lines))
	code (if has-result (drop-last lines) lines)
	result (when has-result (-> lines last (subs 2) trim))]
    {:code code, :result result}))

(defn code-map [node]
  (let [cdata (-> node :content first trim)
	code (subs cdata 9 (- (count cdata) 3))
	lines (split-lines code)]
    (code-map* lines)))

(defn run-code [code-seq]
  ())

(defn- str->istream [str]
  (ByteArrayInputStream. (.getBytes str)))

(defn- ns-html [ns-name]
  (hc/html (instructions ns-name)))

(defn- html-stream [ns-name]
  (str->istream (ns-html ns-name)))

(defn verify-ns [ns-name]
  (let [html (html-resource (html-stream ns-name))
	code-nodes (select html [[:script (attr= :type "syntaxhighlighter")]])
	code-seq (map code-map code-nodes)]
    (run-code code-seq)))
