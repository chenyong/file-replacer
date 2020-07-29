
(ns app.main
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as string]
            [clojure.set :refer [difference intersection union]]
            ["chalk" :as chalk]
            [cljs.core.async :refer [chan <! >! close! go go-loop timeout]]
            [app.tasks :as tasks]
            [cljs-node-io.fs :refer [areaddir areadFile awriteFile afile? adir?]]
            [cumulo-util.core :refer [delay!]]
            [app.tasks.lilac :as tasks-lilac])
  (:require-macros [clojure.core.strint :refer [<<]]))

(defn divide-paths [paths]
  (let [tasks (->> paths
                   (map
                    (fn [x]
                      (go (if (<! (adir? x)) {:dir? true, :path x} {:dir? false, :path x})))))]
    (go
     (loop [xs tasks, files [], folders []]
       (comment println "in the Loop" (first xs) (count xs))
       (if (empty? xs)
         (do (comment println "returnig" [folders files]) [folders files])
         (let [result (<! (first xs))]
           (if (:dir? result)
             (recur (rest xs) files (conj folders (:path result)))
             (recur (rest xs) (conj files (:path result)) folders))))))))

(defn file-filter [x]
  (cond
    (string/ends-with? x "route-configs.ts") false
    (string/includes? x "apis/") false
    (string/ends-with? x ".tsx") true
    (string/ends-with? x ".ts") true
    (string/ends-with? x ".md") false
    (string/ends-with? x ".cljs") true
    :else false))

(defn folder-filter [x]
  (let [ignored #{"node_modules" ".git"}]
    (cond
      (contains? ignored x) false
      (string/starts-with? x ".") false
      (string/ends-with? x ".git") false
      (string/includes? x "/.") false
      :else true)))

(defn process-file! [file on-finish]
  (comment tasks/grab-component-refs! file on-finish)
  (comment tasks/grab-lingual! file on-finish)
  (comment tasks/replace-lodash! file on-finish)
  (comment tasks/unused-lodash! file on-finish)
  (comment tasks/replace-optional-prop! file on-finish)
  (comment tasks-lilac/sort-imports! file on-finish)
  (tasks/dup-semicolon! file on-finish))

(defn process-folder! [base task-runner]
  (go
   (let [[err dirs] (<! (areaddir base))
         children (->> (js->clj dirs) (map (fn [x] (path/join base x))))
         [folders files] (<! (divide-paths children))
         active-files (filter file-filter files)
         active-folders (filter folder-filter folders)
         folder-tasks (->> active-folders
                           (map (fn [folder] (process-folder! folder task-runner)))
                           doall)
         file-tasks (->> active-files
                         (map
                          (fn [file]
                            (go
                             (let [content (<! (areadFile file "utf8"))
                                   write-content! (fn [text]
                                                    (<! (awriteFile file text nil)))]
                               (<! (task-runner file content write-content!))))))
                         doall)]
     (doseq [x file-tasks] (<! x))
     (doseq [x folder-tasks] (<! x)))))

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
  (println (chalk/yellow "Task started"))
  (comment traverse! "." (fn [] (println) (println (.yellow chalk "Task finished"))))
  (go
   (<!
    (process-folder!
     "."
     (fn [filepath content write-content!]
       (go (let [x (rand-int 2000)] (<! (timeout x)) (println filepath (str x "ms")))))))
   (println (chalk/yellow "All finished"))))

(defn main! [] (task!))

(defn reload! [] (js/console.clear) (task!))

(defn replace-file! [file-path content read! write!]
  (comment println file-path)
  (comment tasks/replace-code-import-space! file-path content write!)
  (comment tasks/replace-code-layout! file-path content write!))
