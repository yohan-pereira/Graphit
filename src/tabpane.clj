(ns tabpane
  (:import (javax.swing JTabbedPane)
           (java.awt Dimension))
  (:require [seesaw.core :as s]))

(defn tabpane [page-size]
  (let [panel (s/tabbed-panel)]
    {:panel panel
     :page-size page-size
     :subpanels (atom #{})}))


(defn get-panel [tabpane]
  (:panel tabpane))


(defn child-count [tabpane]
  (count @(:subpanels tabpane)))


(defn rebalance 
  "(re)structures the graphs and tabs."
  [tabpane]
  (.removeAll (get-panel tabpane))
  (s/config! 
    (get-panel tabpane)
    :tabs (map (fn [idx panels]
                 {:title (format "Group %d" idx)
                  :content (s/vertical-panel
                             ;panels is a of the format [[graph index]...] 
                             ;use map to retrive the graphs
                             :items (map first panels))})
               (range 1 (inc (count @(:subpanels tabpane))))
               (partition-all @(:page-size tabpane)
                              (sort-by second @(:subpanels tabpane))))))


(defn add-panel [tabpane panel name]
  (s/invoke-later
    (swap! (:subpanels tabpane) conj [panel name])
    (rebalance tabpane)))


(defn remove-panel [tabpane panel]
  (s/invoke-later
    (swap! (:subpanels tabpane)
           (fn [subpanels]
             (remove #(= (first %) panel)
                     subpanels)))
    (rebalance tabpane)))


(defn cycle-tab [tabpane]
  "cycles the selected tab."
  (s/invoke-later
    (when (> (.getTabCount (get-panel tabpane))
             1)
      (s/selection! (get-panel tabpane)
                    (mod (inc (.getSelectedIndex (get-panel tabpane)))
                         (.getTabCount (get-panel tabpane)))))))
