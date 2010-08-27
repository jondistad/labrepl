(ns verification.runner
  (:use [labrepl.lab :only (instructions)]
	net.cgrand.enlive-html
	[clojure.string :only (trim split-lines join)])
  (:require [hiccup.core :as hc])
  (:import (java.io ByteArrayInputStream)))

(defn code-map* [lines]
  {:post [(string? (:code %))
	  (or (string? (:result %)) (nil? (:result %)))]}
  (let [has-result (re-find #"^->" (last lines))
	code (join "\n" (if has-result (drop-last lines) lines))
	result (when has-result (-> lines last (subs 2) trim))]
    {:code code, :result result}))

(defn code-map [node]
  {:post [(map? %)]}
  (let [cdata (-> node :content first trim)
	code (subs cdata 9 (- (count cdata) 3))
	lines (split-lines code)]
    (code-map* lines)))

(defn run-code [code-seq]
  (let [cur (first code-seq)
	code (:code cur)
	res  (:result cur)
	eres (eval (read-string code))]
    (println "Running:\n" code)
    (when res
      (println "-> " eres)
      (if (or (= (read-string res) eres)
	      (= (eval (read-string res)) eres))
	(println "Correct!")
	(println "Incorrect! Should be: " res)))
    (println)
    (when-let [rem (next code-seq)]
      (recur rem))))

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
