
(ns app.main
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as string]
            [clojure.set :refer [difference intersection]]
            ["chalk" :as chalk])
  (:require-macros [clojure.core.strint :refer [<<]]))

(defonce *counter (atom 0))

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

(defn grab-component-refs! [file-path content write!]
  (let [lines (->> (string/split-lines content)
                   (filter
                    (fn [line]
                      (and (string/includes? line "components=")
                           (not (string/includes? line "import")))))
                   (map string/trim))]
    (when (not (empty? lines))
      (println)
      (println "-------" file-path "------")
      (doseq [line lines] (println line)))))

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

(defn replace-file! [file-path idx content write!]
  (comment println file-path)
  (grab-component-refs! file-path content write!)
  (comment replace-code-import-space! file-path content write!)
  (comment replace-code-layout! file-path content write!))

(defn traverse! [base]
  (let [children (->> (js->clj (fs/readdirSync base)) (map (fn [x] (path/join base x))) set)
        folders (->> children (filter a-folder?) set)
        files (difference children folders)]
    (doseq [x (filter file-filter files)]
      (when (or true (< @*counter 20))
        (swap! *counter inc)
        (comment println (chalk/yellow (<< "File: ~{x}")))
        (replace-file!
         x
         @*counter
         (fs/readFileSync x "utf8")
         (fn [content]
           (println (chalk/red (<< "Writing to ~{x}")))
           (fs/writeFileSync x content)))))
    (doseq [x (filter folder-filter folders)] (traverse! x))))

(defn task! [] (println "Task started") (traverse! ".") (println "Task finished"))

(defn main! [] (task!))

(defn reload! [] (reset! *counter 0) (task!))
