
(ns app.tasks.lilac
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as string]
            [clojure.set :refer [difference intersection]]
            ["chalk" :as chalk]
            [cljs.core.async :refer [chan <! >! close! go go-loop]]
            [app.util :refer [read-file-by-line]]
            [cljs-node-io.fs :refer [areadFile awriteFile]]
            [lilac-parser.core
             :refer
             [replace-lilac
              defparser
              is+
              or+
              optional+
              combine+
              one-of+
              many+
              interleave+
              some+]]
            [lilac-parser.preset :refer [lilac-digit lilac-alphabet]]))

(defn to-nil [xs] nil)

(def space-parser (some+ (is+ " ") to-nil))

(def comma-parser (combine+ [(is+ ",") space-parser] to-nil))

(def variable-parser
  (combine+
   [lilac-alphabet
    (many+ (or+ [lilac-alphabet (one-of+ "_$") lilac-digit]) (fn [xs] (string/join "" xs)))]
   (fn [xs] (string/join "" xs))))

(def import-vars-parser
  (combine+
   [(is+ "import" to-nil)
    space-parser
    (is+ "{" to-nil)
    space-parser
    (interleave+
     variable-parser
     comma-parser
     (fn [xs] (comment println "interleave+" xs) (filter some? xs)))
    space-parser
    (is+ "}" to-nil)]
   (fn [xs] (comment println "combined imports" xs) (nth xs 4))))

(defn sort-imports! [file content write!]
  (go
   (let [replaced (replace-lilac
                   (string/split content "")
                   import-vars-parser
                   (fn [result] (str "import { " (->> result sort (string/join ", ")) " }")))]
     (if (some? (:result replaced))
       (<! (write! (:result replaced)))
       (do (println "Failed" file replaced))))))
