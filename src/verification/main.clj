(ns verification.main
  (:gen-class)
  (:use [verification.runner :only (verify-ns)]
	[clojure.java.io :only (file)])
  (:require [clojure.string :as s])
  (:import (java.io File FilenameFilter)))

(defn lab-namespaces []
  (let [file-filter (reify FilenameFilter
			   (accept
			    [this dir fname]
			    (let [extension (subs fname (- (count fname) 4))]
			      (= extension ".clj"))))
	labs-dir (file (.. Thread currentThread getContextClassLoader (getResource "labs")))
	labs (seq (.list labs-dir file-filter))]
    (map #(s/replace (apply str (drop-last 4 %)) "_" "-") labs)))

(defn validate-labs []
  (doseq [lab (lab-namespaces)]
    (try
      (verify-ns lab)
      (catch Exception e
	(println (str "Error in labs." lab ": " (.getMessage e)))))))

(defn -main [& args]
  (validate-labs))