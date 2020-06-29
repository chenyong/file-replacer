
(ns app.tasks
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as string]
            [clojure.set :refer [difference intersection]]
            ["chalk" :as chalk]
            [cljs.core.async :refer [chan <! >! close! go go-loop]]
            [app.util :refer [read-file-by-line]]
            [cljs-node-io.fs :refer [areadFile awriteFile]]))

(defn grab-component-refs! [file-path on-finish]
  (let [*results (atom [])
        <lines (read-file-by-line file-path)
        show-results! (fn []
                        (if (not (empty? @*results))
                          (do
                           (println)
                           (println (.blue chalk file-path))
                           (println (string/join "\n" @*results)))))
        found-pattern? (fn [line] (string/includes? line "router.go("))]
    (go-loop
     []
     (let [lines (<! <lines)]
       (if (some? lines)
         (do
          (doseq [line lines] (if (found-pattern? line) (swap! *results conj line)))
          (recur))
         (do (show-results!) (on-finish)))))))

(defn grab-lingual! [file on-finish]
  (go
   (let [[err content] (<! (areadFile file "utf8"))
         pieces (->> (re-seq #"(lang|lingual)\.[\w\d]+" content)
                     (map first)
                     (map pr-str)
                     (string/join " "))]
     (println pieces)
     (on-finish))))

(defn replace-code-import-space! [file-path content write!]
  (let [x1 (re-pattern "import \\{\\s+(\\w+\\,?\\s+)*\\} from \"shared/common/layout\";")
        new-text (string/replace
                  content
                  x1
                  (fn [chunk]
                    (let [target (first chunk)]
                      (if (string/includes? target "\n")
                        (-> target
                            (string/replace (re-pattern "\\n\\s*") " ")
                            (string/replace ", }" " }"))
                        target))))]
    (if (not (= new-text content)) (do (println file-path) (comment write! new-text)))))

(defn replace-code-layout! [file-path content write!]
  (let [lines (string/split-lines content)
        x0 "import { "
        x1 " } from \"shared/common/layout\";"
        x2 " } from \"shared/style/preset\";"
        layout-primatives #{"minHeight"
                            "rowCenter"
                            "middleSection"
                            "rowParted"
                            "rowMiddle"
                            "fullHeight"
                            "center"
                            "inlineRow"
                            "column"
                            "row"
                            "fullscreen"
                            "flex"}
        transformed-lines (->> lines
                               (mapcat
                                (fn [line]
                                  (if (and (string/starts-with? line x0)
                                           (string/ends-with? line x1))
                                    (let [xs (set
                                              (string/split
                                               (-> line
                                                   (string/replace x0 "")
                                                   (string/replace x1 ""))
                                               ", "))
                                          layout-ones (intersection xs layout-primatives)
                                          preset-ones (difference xs layout-primatives)
                                          new-lines (filter
                                                     some?
                                                     [(if (empty? layout-ones)
                                                        nil
                                                        (str
                                                         x0
                                                         (string/join ", " layout-ones)
                                                         x1))
                                                      (if (empty? preset-ones)
                                                        nil
                                                        (str
                                                         x0
                                                         (string/join ", " preset-ones)
                                                         x2))])]
                                      new-lines)
                                    [line]))))
        new-content (string/join "\n" transformed-lines)]
    (when (not= (string/trim content) (string/trim new-content))
      (println (chalk/yellow file-path))
      (comment write! new-content))))

(defn replace-content [content replace-dict]
  (if (empty? replace-dict)
    content
    (let [[from to] (first replace-dict)]
      (recur (string/replace content from to) (rest replace-dict)))))

(defn replace-lodash! [file on-finish]
  (go
   (let [[err content] (<! (areadFile file "utf8"))
         pieces (if (string/includes? content "import _ from \"lodash\"")
                  (->> (re-seq #"_\.\w+\(" content)
                       (filter (fn [piece] (not (contains? #{"_.go(" "_.path("} piece))))
                       (distinct))
                  nil)]
     (when (and (some? pieces) (not (empty? pieces)))
       (let [variables (->> pieces (map (fn [piece] (subs piece 2 (dec (count piece))))))
             import-line (str
                          "import { "
                          (string/join ", " variables)
                          " } from \"lodash-es\";")
             replace-dict (->> pieces
                               (map (fn [piece] [piece (subs piece 2)]))
                               (concat [["import _ from \"lodash\"" import-line]]))]
         (println "replacing" file)
         (<! (awriteFile file (replace-content content replace-dict) nil))))
     (on-finish))))

(defn replace-optional-prop! [file on-finish]
  (go
   (let [[err content] (<! (areadFile file "utf8"))
         has-safe-get? (string/includes? content "safeGet")]
     (when has-safe-get?
       (doseq [line (string/split content "\n")]
         (when (string/includes? line "safeGet3(")
           (let [new-line (string/replace
                           line
                           #"safeGet3\(([\w\d\[\]\.]+)\,\s\"([\w\d]+)\"\,\s([\w\d]+)\,\s\"([\w\d]+)\"\)"
                           "$1?.$2?.[$3]?.$4")]
             (when (not= line new-line) (println) (println line) (println new-line)))))
       (comment
        let
        ((new-content
          (string/replace
           content
           #"safeGet3\(([\w\d\[\]\.]+)\,\s\"([\w\d]+)\"\,\s([\w\d]+)\,\s\"([\w\d]+)\"\)"
           "$1?.$2?.[$3]?.$4")))
        (when (not= content new-content)
          (println (chalk/red "replacing" file))
          (<! (awriteFile file new-content nil))))
       (let [new-content (string/replace
                          content
                          "import { safeGet, safeGet3 } from \"@jimengio/safe-property\";\n"
                          "")]
         (when (not= content new-content)
           (println (chalk/red "replacing" file))
           (<! (awriteFile file new-content nil)))))
     (on-finish))))

(defn unused-lodash! [file on-finish]
  (go
   (let [[err content] (<! (areadFile file "utf8"))
         has-import? (string/includes? content "import _ from \"lodash\"")
         not-used? (empty?
                    (if has-import?
                      (->> (re-seq #"_\.\w+\(" content)
                           (filter
                            (fn [piece] (not (contains? #{"_.go(" "_.path("} piece)))))
                      nil))]
     (when (and has-import? not-used?)
       (println "replacing" file)
       (<! (awriteFile file (string/replace content "import _ from \"lodash\";\n" "") nil)))
     (on-finish))))
