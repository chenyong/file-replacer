
(ns app.tasks
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as string]
            [clojure.set :refer [difference intersection]]
            ["chalk" :as chalk]
            [cljs.core.async :refer [chan <! >! close! go go-loop]]))

(defn grab-component-refs! [file-path content read! write!]
  (let [lines (->> (string/split-lines content)
                   (filter (fn [line] (string/includes? line "CSS")))
                   (map string/trim))]
    (when (not (empty? lines)) (println (.blue chalk file-path)) (println lines)))
  (read!))

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
