
(ns app.main
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as string]
            [clojure.set :refer [difference]]
            ["chalk" :as chalk])
  (:require-macros [clojure.core.strint :refer [<<]]))

(defonce *counter (atom 0))

(defn file-filter [x]
  (cond
    (string/starts-with? x ".") false
    (string/ends-with? x ".tsx") true
    (string/ends-with? x ".ts") true
    :else false))

(defn folder-filter [x]
  (let [ignored #{"node_modules" ".git"}]
    (cond (contains? ignored x) false (string/starts-with? x ".") false :else true)))

(defn replace-file! [idx content write!] (comment println "content" (subs content 0 200)))

(defn traverse! [base]
  (let [children (->> (js->clj (fs/readdirSync base)) (map (fn [x] (path/join base x))) set)
        folders (->> children
                     (filter (fn [x] (let [stat (fs/statSync x)] (.isDirectory stat))))
                     set)
        files (difference children folders)]
    (doseq [x (filter file-filter files)]
      (when (or true (< @*counter 10))
        (swap! *counter inc)
        (println (chalk/yellow (<< "File: ~{x}")))
        (replace-file!
         @*counter
         (fs/readFileSync x "utf8")
         (fn [content] (println "Writing to" x) (comment fs/writeFileSync x content)))))
    (doseq [x (filter folder-filter folders)] (traverse! x))))

(defn task! [] (println "Running task") (traverse! "."))

(defn main! [] (task!))

(defn reload! [] (reset! *counter 0) (task!))
