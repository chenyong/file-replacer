
(ns app.util
  (:require ["fs" :as fs]
            ["path" :as path]
            [clojure.string :as string]
            ["chalk" :as chalk]
            [cljs.core.async :refer [chan <! >! close! go go-loop put!]]))

(defn read-file-by-line [file]
  (let [<lines (chan), readable (fs/createReadStream file)]
    (.on readable "data" (fn [chunk] (put! <lines (string/split chunk "\n"))))
    (.on readable "end" (fn [] (close! <lines)))
    <lines))
