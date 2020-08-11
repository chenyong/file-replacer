
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
              find-lilac
              defparser
              is+
              or+
              optional+
              combine+
              one-of+
              many+
              interleave+
              some+
              other-than+]]
            [lilac-parser.preset :refer [lilac-digit lilac-alphabet]]))

(defn to-nil [xs] nil)

(def space-parser (some+ (is+ " ") to-nil))

(def comma-parser (combine+ [(is+ ",") space-parser] to-nil))

(def lilac-comma (combine+ [(is+ ",") (some+ (is+ " "))] (fn [xs] ", ")))

(def lilac-float
  (combine+
   [(many+ lilac-digit (fn [xs] (string/join "" xs)))
    (optional+
     (combine+
      [(is+ ".") (many+ lilac-digit (fn [xs] (string/join "" xs)))]
      (fn [xs] (string/join "" xs)))
     (fn [xs] (str xs)))]
   (fn [xs] (string/join "" xs))))

(def lilac-hex (one-of+ "0123456789abcdefABCDEF"))

(def lilac-colors
  (or+
   [(combine+
     [(is+ "#")
      lilac-hex
      lilac-hex
      lilac-hex
      (optional+ (combine+ [lilac-hex lilac-hex lilac-hex] (fn [xs] (string/join "" xs))))]
     (fn [xs] (string/join "" xs)))
    (combine+
     [(is+ "rgb(") lilac-float lilac-comma lilac-float lilac-comma lilac-float (is+ ")")]
     (fn [xs] (string/join "" xs)))
    (combine+
     [(is+ "rgba(")
      lilac-float
      lilac-comma
      lilac-float
      lilac-comma
      lilac-float
      lilac-comma
      lilac-float
      (is+ ")")]
     (fn [xs] (string/join "" xs)))
    (combine+
     [(is+ "hsl(")
      lilac-float
      lilac-comma
      lilac-float
      (is+ "%")
      lilac-comma
      lilac-float
      (is+ "%)")]
     (fn [xs] (string/join "" xs)))
    (combine+
     [(is+ "hsla(")
      lilac-float
      lilac-comma
      lilac-float
      (is+ "%")
      lilac-comma
      lilac-float
      (is+ "% ")
      lilac-comma
      lilac-float
      (is+ ")")]
     (fn [xs] (string/join "" xs)))]))

(defn find-colors! [filepath content write!]
  (go
   (let [result (find-lilac content lilac-colors)]
     (when (not (empty? (:result result))) (println (pr-str (map :value (:result result))))))))

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

(def lilac-equals
  (combine+
   [(is+ " == " (fn [] nil))
    (or+
     [(many+ lilac-alphabet (fn [x] (string/join "" x)))
      (many+ lilac-digit (fn [x] (string/join "" x)))
      (combine+
       [(is+ "\"" (fn [x] "\""))
        (many+ (other-than+ "\"") (fn [x] (string/join "" x)))
        (is+ "\"" (fn [x] "\""))]
       (fn [x] (string/join "" x)))]
     (fn [x] x))]
   (fn [x] (last x))))

(defn more-optional! [filepath content write!]
  (go
   (let [lilac-variable (many+
                         (or+ [lilac-alphabet lilac-digit (one-of+ ".?[]")])
                         (fn [xs] (string/join "" xs)))
         lilac-pattern (combine+
                        [lilac-variable
                         (is+ " && " (fn [x] nil))
                         lilac-variable
                         (or+ [(is+ ".") (is+ "(") (is+ "[")])]
                        (fn [xs]
                          (comment println "xs" (pr-str xs))
                          {:from (nth xs 0), :to (nth xs 2), :suffix (nth xs 3)}))]
     (when (string/includes? content " && ")
       (let [changed (->> (string/split content "\n")
                          (map
                           (fn [line]
                             (if (string/includes? line " && ")
                               (:result
                                (replace-lilac
                                 line
                                 lilac-pattern
                                 (fn [info]
                                   (let [from (string/replace (:from info) "?" "")
                                         to (:to info)]
                                     (if (string/starts-with? to from)
                                       (if (= to from)
                                         (let [replaced (str (:from info) "?." (:suffix info))]
                                           (println "\n" "\n" "=====Replaced:")
                                           (println replaced)
                                           (println "info" info (pr-str line))
                                           replaced)
                                         (let [replaced (str
                                                         (:from info)
                                                         "?"
                                                         (string/replace to from "")
                                                         (:suffix info))]
                                           (println "\n" "\n" "=====Replaced:")
                                           (println replaced)
                                           (println "info" info (pr-str line))
                                           replaced))
                                       (str (:from info) " && " (:to info) (:suffix info)))))))
                               line)))
                          (string/join "\n"))
             changed (if (string/ends-with? content "\n") (str changed "\n") changed)]
         (when (not= changed content) (println "Changed:" filepath) (<! (write! changed))))))))

(defn replace-equals! [filepath content write!]
  (go
   (let [result (find-lilac content lilac-equals)]
     (when (not (empty? (->> (:result result) (remove (fn [x] (or (= (:value x) "null")))))))
       (println "replacing" filepath (pr-str (:result result)))
       (let [info (replace-lilac
                   content
                   lilac-equals
                   (fn [x]
                     (case x "null" " == null" "undefined" " == null" (str " === " x))))]
         (<! (write! (:result info))))))))

(def time-format-lilac
  (combine+
   [(is+ ".format(\"") (many+ (other-than+ "\"") (fn [xs] (string/join "" xs))) (is+ "\")")]
   (fn [xs] (nth xs 1))))

(defn replace-time-format! [filepath content write!]
  (go
   (let [result (find-lilac content time-format-lilac)]
     (when (not (empty? (:result result))) (println (pr-str (map :value (:result result))))))))

(defn sort-imports! [file content write!]
  (go
   (let [replaced (replace-lilac
                   (string/split content "")
                   import-vars-parser
                   (fn [result] (str "import { " (->> result sort (string/join ", ")) " }")))]
     (if (some? (:result replaced))
       (<! (write! (:result replaced)))
       (do (println "Failed" file replaced))))))
