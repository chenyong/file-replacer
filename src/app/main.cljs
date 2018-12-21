
(ns app.main
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as string]
            [clojure.set :refer [difference intersection]]
            ["chalk" :as chalk]
            [cljs.core.async :refer [chan <! >! close! go go-loop]]
            [app.tasks :as tasks])
  (:require-macros [clojure.core.strint :refer [<<]]))

(defn a-folder? [x] (let [stat (fs/statSync x)] (.isDirectory stat)))

(defn file-filter [x]
  (cond
    (string/ends-with? x "route-configs.ts") false
    (string/includes? x "apis/") false
    (string/ends-with? x ".tsx") true
    (string/ends-with? x ".ts") false
    :else false))

(defn folder-filter [x]
  (let [ignored #{"node_modules" ".git"}]
    (cond (contains? ignored x) false (string/starts-with? x ".") false :else true)))

(defn replace-file! [file-path content read! write!]
  (comment println file-path)
  (tasks/grab-component-refs! file-path content read! write!)
  (comment tasks/replace-code-import-space! file-path content write!)
  (comment tasks/replace-code-layout! file-path content write!))

(defn process-file! [file]
  (let [<task (chan)
        finish! (fn [] (println "finish processing") (go (>! <task file) (close! <task)))]
    (println "processing file:" file)
    (fs/readFile
     file
     "utf8"
     (fn [err content]
       (println "file read")
       (replace-file!
        file
        content
        finish!
        (fn [content]
          (println (chalk/red (<< "Writing to ~{file}")))
          (fs/writeFile file content (fn [err] (finish!)))))))
    <task))

(defn traverse! [base]
  (let [<task (chan)]
    (fs/readdir
     base
     (fn [err dirs]
       (let [children (->> (js->clj dirs) (map (fn [x] (path/join base x))) set)
             folders (->> children (filter a-folder?) set)
             files (difference children folders)]
         (go
          (doseq [x (filter file-filter files)]
            (comment println (chalk/yellow (<< "File: ~{x}")))
            (>! <task x)))
         (doseq [x (filter folder-filter folders)] (go (>! <task (<! (traverse! x))))))))
    <task))

(defn task! []
  (println "Task started")
  (let [<files (traverse! "."), <processing (chan), *c (atom 0)]
    (go-loop
     []
     (let [file (<! <files)]
       (println "got file to process:" file)
       (<! (process-file! file))
       (recur)))
    (go-loop
     []
     (let [file (<! <processing)] (println "done processing:" file))
     (swap! *c dec)
     (if (zero? @*c)
       (do (close! <files) (close! <processing) (println "Task finished"))
       (recur)))))

(defn main! [] (task!))

(defn reload! [] (task!))
