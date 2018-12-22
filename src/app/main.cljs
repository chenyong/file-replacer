
(ns app.main
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as string]
            [clojure.set :refer [difference intersection]]
            ["chalk" :as chalk]
            [cljs.core.async :refer [chan <! >! close! go go-loop]]
            [app.tasks :as tasks]
            [cljs-node-io.fs :refer [areaddir areadFile awriteFile afile? adir?]]
            [cumulo-util.core :refer [delay!]])
  (:require-macros [clojure.core.strint :refer [<<]]))

(defn divide-paths [xs0]
  (let [<result (chan)]
    (go-loop
     [xs xs0 files [] folders []]
     (comment println "in the Loop" (first xs) (count xs))
     (if (pos? (count xs))
       (if (<! (adir? (first xs)))
         (recur (rest xs) files (conj folders (first xs)))
         (recur (rest xs) (conj files (first xs)) folders))
       (>! <result [folders files])))
    <result))

(defn file-filter [x]
  (or true
      (cond
        (string/ends-with? x "route-configs.ts") false
        (string/includes? x "apis/") false
        (string/ends-with? x ".tsx") true
        (string/ends-with? x ".ts") false
        :else false)))

(defn folder-filter [x]
  (let [ignored #{"node_modules" ".git"}]
    (cond (contains? ignored x) false (string/starts-with? x ".") false :else true)))

(defn replace-file! [file-path content read! write!]
  (comment println file-path)
  (tasks/grab-component-refs! file-path content read! write!)
  (comment tasks/replace-code-import-space! file-path content write!)
  (comment tasks/replace-code-layout! file-path content write!))

(defn process-file! [file call-next]
  (fs/readFile
   file
   "utf8"
   (fn [err content]
     (replace-file!
      file
      content
      call-next
      (fn [content]
        (println (chalk/red (<< "Writing to ~{file}")))
        (fs/writeFile file content (fn [err] (call-next))))))))

(defn read-loop [*resource on-continue]
  (if (empty? @*resource)
    (delay!
     0.4
     #(if (empty? @*resource) (println "Give up.") (read-loop *resource on-continue)))
    (let [file (first @*resource)]
      (on-continue file #(read-loop *resource on-continue))
      (swap! *resource disj file))))

(defn traverse! [base on-collect]
  (go
   (comment println "starting the block")
   (let [[err dirs] (<! (areaddir base))
         children (->> (js->clj dirs) (map (fn [x] (path/join base x))))
         [folders files] (<! (divide-paths children))]
     (comment println "read about children")
     (doseq [x (filter file-filter files)]
       (comment println (chalk/yellow (<< "File: ~{x}")))
       (on-collect x))
     (doseq [y (filter folder-filter folders)] (traverse! y on-collect)))))

(defn task! []
  (let [*file-collection (atom #{})]
    (println "Task started")
    (traverse! "." (fn [file] (swap! *file-collection conj file)))
    (comment process-file! file (fn [] (println "call nexted")))
    (read-loop
     *file-collection
     (fn [file on-next]
       (comment println "read file" file)
       (process-file! file (fn [] (on-next)))))))

(defn main! [] (task!))

(defn reload! [] (task!))
