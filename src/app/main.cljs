
(ns app.main
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as string]
            [clojure.set :refer [difference intersection union]]
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
  (cond
    (string/ends-with? x "route-configs.ts") false
    (string/includes? x "apis/") false
    (string/ends-with? x ".tsx") true
    (string/ends-with? x ".ts") false
    (string/ends-with? x ".md") true
    :else false))

(defn folder-filter [x]
  (let [ignored #{"node_modules" ".git"}]
    (cond
      (contains? ignored x) false
      (string/starts-with? x ".") false
      (string/ends-with? x ".git") false
      (string/includes? x "/.") false
      :else true)))

(defn process-file! [file on-finish] (tasks/grab-component-refs! file on-finish))

(defn traverse! [base on-finish]
  (go
   (let [[err dirs] (<! (areaddir base))
         children (->> (js->clj dirs) (map (fn [x] (path/join base x))))
         [folders files] (<! (divide-paths children))
         active-files (filter file-filter files)
         active-folders (filter folder-filter folders)
         *pending (atom (union (set active-files) (set active-folders)))
         check-finished! (fn []
                           (if (empty? @*pending)
                             (do
                              (comment println (.blue chalk (<< "Finished ~{base}")))
                              (on-finish))))
         count-finished (fn [x]
                          (swap! *pending disj x)
                          (comment println (.gray chalk base))
                          (check-finished!))]
     (doseq [x active-files]
       (comment println (chalk/yellow (<< "File: ~{x}")))
       (process-file! x #(count-finished x)))
     (doseq [y active-folders] (traverse! y #(count-finished y)))
     (check-finished!))))

(defn task! []
  (println)
  (println)
  (println (.yellow chalk "Task started"))
  (traverse! "." (fn [] (println) (println (.yellow chalk "Task finished")))))

(defn main! [] (task!))

(defn reload! [] (js/console.clear) (task!))

(defn replace-file! [file-path content read! write!]
  (comment println file-path)
  (comment tasks/replace-code-import-space! file-path content write!)
  (comment tasks/replace-code-layout! file-path content write!))
