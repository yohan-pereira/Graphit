(ns graphit
  (:import (org.jfree.chart ChartPanel JFreeChart ChartFactory)
           (org.jfree.data.xy XYSeries XYSeriesCollection)
           (org.jfree.chart.plot PlotOrientation)
           (org.jfree.chart.axis NumberAxis)
           (org.jfree.chart.labels XYToolTipGenerator)
           (org.jfree.ui ApplicationFrame)
           (java.io BufferedReader PrintWriter File)
           (java.text NumberFormat DecimalFormat SimpleDateFormat)
           (java.util Date)
           (java.net ServerSocket Socket InetSocketAddress)
           (java.awt.event ItemEvent)
           (java.awt BasicStroke Dimension Color)
           (java.nio ByteBuffer)
           (java.nio.channels Selector ServerSocketChannel SelectionKey))
  (:use clojure.java.io)
  (:require tabpane
            [clojure.tools.cli :refer [parse-opts]]
            [seesaw.border :as sb] 
            [seesaw.core :as s])
  (:gen-class))


;; Thread-shared vars
(def ^:dynamic *redraw-delay-ms* (atom 10000))

(def ^:dynamic *data-gatherer* (agent []))
(def ^:dynamic *expire-threshold* (atom (Integer/MAX_VALUE)))
(def ^:dynamic *max-readings* (atom 120))
(def ^:dynamic *graphs-per-page* (atom 4))

(def ^:dynamic *tab-cycle-active* (atom false))
(def ^:dynamic *tab-cycle-delay* (atom 30))

(def ^:dynamic *graphs* (atom {}))
(def ^:dynamic *hide-legend* (atom nil))

(def ^:dynamic *datastream-listeners* (agent #{}))


;; Objects used for interruptible sleeps
(def ^:dynamic *plot-alarm* (Object.))
(def ^:dynamic *tab-cycle-alarm* (Object.))


;;; Misc utilities

(defn interruptible-sleep [ms alarm]
  (locking alarm
    (if (pos? ms)
      (.wait alarm ms)
      (.wait alarm))))

(defn interrupt-sleep [alarm]
  (locking alarm
    (.notify alarm)))


;;; Ah, swing...

(def ^:dynamic *window*
     {:frame (s/frame :title "Graphit"
                      :minimum-size [800 :by 600]
                      :on-close :exit)
      :panel (tabpane/tabpane *graphs-per-page*)})


(defn set-rate
  "Set the value of `atom' to `n' and interrupt `alarm' to wake up its owner."
  [atom n alarm]

  (reset! atom n)
  (interrupt-sleep alarm))


(defn parse-time [time timefmt]
  (if (string? time)
    (if timefmt
      (.. (SimpleDateFormat. timefmt)
          (parse time)
          getTime)
      (BigDecimal. time))
    time))


(defn parse-datapoint [#^String s]
  (try
   (let [[graph & bits] (.split s "\t")
         [time label num timefmt] (if (= (count bits) 2)
                                    (concat [(System/currentTimeMillis)] bits)
                                    bits)]
     {:graph graph
      :time (parse-time time timefmt)
      :line label
      :source s
      :value (if (= num "delete")
               num
               (.parse (NumberFormat/getInstance) num))})
   (catch Exception e
     (.println System/err (format "Failed to parse line: '%s'" s))
     (throw e))))


(defn add-datapoint [point]
  (send *data-gatherer* conj point)
  (send-off *datastream-listeners*
            (fn [listeners]
              (reduce (fn [alive-listeners listener]
                        (try
                         (let [msg (ByteBuffer/wrap
                                    (.getBytes (str (:source point) "\n")))]
                           (if (= (.write listener msg)
                                  (.limit msg))
                             (conj alive-listeners listener)
                             alive-listeners))
                         (catch Exception e
                           alive-listeners)))
                      #{}
                      listeners))))


(defmacro print-exceptions [& body]
  `(try ~@body
        (catch Exception e#
          (.printStackTrace e#)
          (.printStackTrace (.getCause e#)))))


(defn make-number-formatter [fmt]
  (proxy [DecimalFormat] []
    (format [a b c]
      (.append b (format fmt a)))))


(defn make-time-formatter [fmt]
  (proxy [DecimalFormat] []
    (format [a b c]
      (let [formatter (SimpleDateFormat. fmt)
            ;; Any number bigger than the number of milliseconds since
            ;; 1990-01-01 is interpreted as a time stamp.
            time-indicator 631112400000]
        (if (>= a time-indicator)
          (.append b (.format formatter (Date. (long a))))
          (proxy-super format a b c))))))


(defn dump-dataset [wrtr dataset graph-name]
  (doseq [i (range 0 (.getSeriesCount dataset))]
    (let [key (.getSeriesKey dataset i)
          series (.getSeries dataset key)]
      (doseq [data-item (.getItems series)]
        (.println wrtr
                  (format "%s\t%f\t%s\t%f"
                          graph-name
                          (double (.getX data-item))
                          key
                          (double (.getY data-item))))))))


(defn dump-state [filename]
  (with-open [fh (writer filename)]
    (doseq [[name graph] @*graphs*]
      (dump-dataset fh (:dataset graph) name))
    (.flush fh)))


(defn make-tooltip-generator [number-formatter]
  (proxy [XYToolTipGenerator] []
    (generateToolTip [dataset series item]
      (format "%s, %s, %s"
              (.getSeriesKey dataset series)
              (.format number-formatter
                       (.getXValue dataset series item))
              (String/valueOf (.getYValue dataset series item))))))


(defn make-chart [title dataset & [opts]]
  (let [y-axis (or (get opts "y-label") "")
        linechart (if (get opts "scatter")
                    (ChartFactory/createScatterPlot title
                                                    ""
                                                    y-axis
                                                    dataset
                                                    PlotOrientation/VERTICAL
                                                    true true false)
                    (ChartFactory/createXYLineChart title
                                                    ""
                                                    y-axis
                                                    dataset
                                                    PlotOrientation/VERTICAL
                                                    true true false))
        formatter (if (get opts "format-string")
                    (make-number-formatter (get opts "format-string"))
                    (make-time-formatter (or (get opts "time-format")
                                             "yyyy-MM-dd HH:mm:ss")))]
    (when @*hide-legend*
      (.setVisible (.getLegend linechart)
                   false))
    (.setAntiAlias linechart true)

    ;; Initialisation gumpf
    (let [xyplot (.getPlot linechart)]
      (doto xyplot
        (.setDomainPannable true)
        (.setRangePannable true)
        (.setDomainZeroBaselineVisible true)
        (.setRangeZeroBaselineVisible true)
        (.setBackgroundPaint (Color. 239 239 239)))

      (doto (.getRenderer xyplot)
        (.setBaseToolTipGenerator (make-tooltip-generator formatter))
        (.setStroke (BasicStroke. 2.0))
        (.setBaseShapesVisible true)
        (.setBaseShapesFilled true)
        (.setOutlineStroke (BasicStroke. 3.0))
        (.setDrawOutlines true))

      (doto (.getRangeAxis xyplot)
        (.setAutoRangeIncludesZero false)
        (.setStandardTickUnits (NumberAxis/createIntegerTickUnits)))

      (doto (.getDomainAxis xyplot)
        (.setNumberFormatOverride formatter)))

    (doto (ChartPanel. linechart)
      (.setMaximumDrawWidth Integer/MAX_VALUE)
      (.setMaximumDrawHeight Integer/MAX_VALUE)
      (.setInitialDelay 50)
      (.setMouseWheelEnabled true))))


(defn hide-graph
  ([graphname] (hide-graph graphname false))
  ([graphname force]
     (when (or force (> (tabpane/child-count (:panel *window*)) 1))
       (let [chart (:chart (@*graphs* graphname))]
         (tabpane/remove-panel (:panel *window*) chart)))))


(defn hide-all-graphs
  ([] (hide-all-graphs false))
  ([force]
     (doseq [graph (keys @*graphs*)]
       (hide-graph graph force))))


(defn show-graph [graphname]
  (hide-graph graphname true)
  (let [chart (:chart (@*graphs* graphname))]
    (tabpane/add-panel (:panel *window*) chart graphname)))


(defn show-all-graphs []
  (doseq [graph (keys @*graphs*)]
    (show-graph graph)))


(defn remove-graph
  "Removes 'graphname' from the display."
  [graphname]
  (when (= (tabpane/child-count (:panel *window*)) 1)
    ;; show any other available graphs...
    (show-all-graphs))
  (hide-graph graphname true)
  (swap! *graphs* dissoc graphname))

(defn menu-item [label f]
  (s/menu-item :text label
               :listen [:action f]))


;; A terrible hack ;o)
(defn instrument-popup-menu [name]
  (let [chart (:chart (@*graphs* name))
        field (.getDeclaredField (class chart) "popup")]
    (.setAccessible field true)
    (doto (.get field chart)
      (.addSeparator)
      (.add (menu-item "Show only this graph"
                       (fn [_]
                         (hide-all-graphs true)
                         (show-graph name))))
      (.add (menu-item "Hide this graph"
                       (fn [_] (hide-graph name))))
      (.add (menu-item "Show all graphs"
                       (fn [_] (show-all-graphs))))
      (.addSeparator)
      (.add (menu-item "Delete this graph"
                       (fn [_]
                         (when (s/confirm
                                   (:frame *window*)
                                   "Really delete?"
                                 :option-type :yes-no
                                 :type :warning)
                           (remove-graph name))))))))


(defn series-seq [xyseries]
  (map #(.getDataItem xyseries %)
       (range (.getItemCount xyseries))))


(defn delete-line [graph-name label]
  (let [graph (@*graphs* graph-name)
        line (get-in graph [:lines label])]
    (.removeSeries (:dataset graph) line)
    (swap! *graphs* update-in
           [graph-name :lines]
           dissoc label)))


(defn add-empty-chart [{:keys [title options]}]
  (let [dataset (XYSeriesCollection.)
        chart (make-chart title dataset options)]
    (tabpane/add-panel (:panel *window*) chart title)
    (swap! *graphs* assoc title
           {:chart chart
            :dataset dataset
            :lines {}})
    (instrument-popup-menu title)))


(defn do-plot [values]
  (s/invoke-later
    (print-exceptions

      (doseq [{:keys [graph time line value]} values]

        (when-not (@*graphs* graph)
          (add-empty-chart {:title graph :options {}}))

        (when-not (get-in @*graphs* [graph :lines line])
          (let [new-line (doto (XYSeries. line)
                           (.setMaximumItemCount @*max-readings*))]
            (swap! *graphs* update-in
                   [graph :lines]
                   assoc line new-line)

            (.addSeries (get-in @*graphs* [graph :dataset])
                        new-line)))

        (if (= value "delete")
          (delete-line graph line)
          (.add #^XYSeries (get-in @*graphs* [graph :lines line])
                #^Number time #^Number value false)))

      (doseq [[graph-name graph] @*graphs*]
        (doseq [[label line] (:lines graph)
                :let [last-reading (.getMaxX line)]]
          ;; Experimental: if any line has had *expire-threshold*
          ;; data points since this line's most recent datapoint, "expire" this
          ;; line.
          (when (some (fn [line]
                        (> (count (filter #(> (.getXValue %)
                                              last-reading)
                                          (series-seq line)))
                           @*expire-threshold*))
                      (vals (:lines graph)))
            (delete-line graph-name label))))

      (doseq [graph (vals @*graphs*)]
        (when-let [series (first (-> graph :lines vals))]
          (.fireSeriesChanged series)))

      (send-off *data-gatherer* do-plot)))
  (interruptible-sleep @*redraw-delay-ms* *plot-alarm*)
  [])


(defn save-state []
  (dump-state (File. (System/getenv "HOME") ".graphit.state")))


(defn parse-create-cmd [s]
  (let [[_ title & opts] (.split s "\t")
        opts (apply hash-map opts)]
    {:title title
     :options opts}))


(defn handle-client [#^Socket client]
  (print-exceptions
   (with-open [#^BufferedReader in (reader (.getInputStream client))
               #^PrintWriter out (writer (.getOutputStream client))]
     (loop []
       (let [line (.readLine in)]
         (when (and line (not= line "done"))
           (cond (= line "help")
                 (do (.println out "Syntax: graph name:[xval]:line name:(yval|\"delete\")")
                     (.println out "Exit with 'done'")
                     (.flush out))

                 (= line "cycle")
                 (interrupt-sleep *tab-cycle-alarm*)

                 (= line "dump")
                 (save-state)

                 (.startsWith line "create")
                 (let [cmd (parse-create-cmd line)]
                   (when-not (@*graphs* (:title cmd))
                     (add-empty-chart cmd)))

                 (= line "") nil
                 :else (add-datapoint (parse-datapoint line)))
           (recur))))))
  (.close client))


(defn handle-inputs
  "Open a socket and read lines of input."
  [port]
  (try
   (with-open [server (ServerSocket. port)]
     (println "Listening on" port)
     (while true
            (print-exceptions
             (let [client (.accept server)]
               (future (handle-client client))))))
   (catch Exception e
     (.printStackTrace e)
     (System/exit 1))))


(defn make-control-panel 
  "creates the settings panel that is rendered above the graphs."
  []
  (s/flow-panel
    :align :left
    :items [(s/label "Redraw rate:")
            (s/text
              :text (str @*redraw-delay-ms*)
              :columns 6
              :listen [:action (fn [evt]
                                 (set-rate
                                   *redraw-delay-ms*
                                   (Integer/parseInt (.getActionCommand evt))
                                   *plot-alarm*))])
            (s/label "ms")

            (s/separator :size [1 :by 20])

            ;; Tab cycling adjustment
            (s/checkbox
              :text "Cycle tabs every"
              :listen [:item #(try
                                (reset! *tab-cycle-active*
                                        (= (.getStateChange %) ItemEvent/SELECTED))
                                (catch Exception _)) ])

            (s/text 
              :text (str @*tab-cycle-delay*) 
              :columns 3
              :listen [:action (fn [evt]
                                 (set-rate
                                   *tab-cycle-delay*
                                   (Integer/parseInt (.getActionCommand evt))
                                   *tab-cycle-alarm*))])
            (s/label " secs")

            (s/separator :size [1 :by 20])

            ;; Graphs per page
            (s/label "Show ")
            (s/text 
              :text (str @*graphs-per-page*) 
              :columns 3
              :listen [:action (fn [evt]
                                 (reset! *graphs-per-page*
                                         (Integer. (.getActionCommand evt)))
                                 (tabpane/rebalance (:panel *window*)))])
            (s/label " graphs/page")]))


(defn make-status-bar 
  "Creates the status bar that is rendered above the graphs, 
  which contains the control panel and the dump points button"
  []
  (s/border-panel 
    :west (make-control-panel)
    :east (s/button :text "Dump points"
                    :listen [:action (fn [_]
                                       (try (save-state)
                                            (catch Exception _)))])
    :border (sb/empty-border :thickness 1) 
    :size [15 :by 33]))


(defn run-ui [geometry]
  "initlises the ui content."
  (s/invoke-later
    (doto (:frame *window*)
      (s/config! :content (s/border-panel
                            :north (make-status-bar)
                            :center (tabpane/get-panel (:panel *window*))))
      ;does not work
      ;(s/listen :window-closed (fn [_] (save-state)))
      (s/config! :size geometry)
      (s/show!))))


(defn datastream-handler [port]
  (try
   (let [ssc (ServerSocketChannel/open)
         selector (Selector/open)]
     (doto ssc
       (.. socket (bind (InetSocketAddress. port)))
       (.configureBlocking false)
       (.register selector SelectionKey/OP_ACCEPT))

     (while true
       (.select selector)

       (let [keyset (.. selector selectedKeys iterator)]
         (doseq [key (iterator-seq keyset)]
           (.remove keyset)
           (let [client (.accept ssc)]
             (.configureBlocking client false)
             (send-off *datastream-listeners* conj client))))))
   (catch Exception e
     (.printStackTrace e)
     (System/exit 1))))



(defn parse-geometry [s]
  (try
   (let [[w h] (map #(Integer/valueOf %) (.split s "[xX]"))]
     [w :by h])
   (catch Exception _
     [1280 :by 1024])))

(def cli-options
  [[nil "--max-to-keep ARG" "Maximum points to keep per line."
    :default 120
    :parse-fn #(Integer/parseInt %)]

   [nil "--expire-threshold ARG" "The number of points a line can fall behind other lines before being expired."
    :default nil
    :parse-fn #(Integer/parseInt %)]

   [nil "--hide-legend" "Don't display the graph's legend."]

   [nil "--graphs-per-page ARG" "Number of graphs per tabbed page."
    :default 4
    :parse-fn #(Integer/parseInt %)]

   ["-p" "--port PORT" "Listen port"
    :default 6666
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]]

   [nil "--datastream-port PORT" "Data stream port"
    :default nil
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]]

   [nil "--redraw ARG" "Redraw ms."
    :default 2000
    :parse-fn #(Integer/parseInt %)]

   [nil "--geometry WxH" "Window dimensions (WxH)"
    :default [1280 :by 1024]
    :parse-fn parse-geometry]

   ["-h" "--help"]])

(defn usage 
  "generates the help text"
  [options]
  (clojure.string/join \newline ["A handy graphing thingy." 
                                 options]))
(defn error-msg [errors]
  (str "Invalid arguments. Run with --help to see available arguments.\n\n"
       (clojure.string/join \newline errors)))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn -main [& args]
  (let [{:keys [errors options summary]} (parse-opts args cli-options)
        {:keys [max-to-keep
                expire-threshold
                hide-legend 
                graphs-per-page
                port
                datastream-port
                redraw
                geometry]}                options]
    (cond
      (:help options) (exit 0 (usage summary))
      errors (exit 1 (error-msg errors)))    

    (.addShutdownHook (Runtime/getRuntime)
                      (Thread. save-state))

    (set-rate *redraw-delay-ms* (Integer. redraw) *plot-alarm*)
    (when expire-threshold
      (reset! *expire-threshold* (Integer. expire-threshold)))
    (reset! *max-readings* (Integer. max-to-keep))
    (reset! *hide-legend* hide-legend)
    (reset! *graphs-per-page* (Integer. graphs-per-page))
    (future (handle-inputs (Integer/valueOf port)))

    (when datastream-port
      (future (datastream-handler (Integer/valueOf datastream-port))))

    (send-off *data-gatherer* do-plot)
    (future
     (while true
       (when @*tab-cycle-active*
         (tabpane/cycle-tab (:panel *window*)))
       (interruptible-sleep (* @*tab-cycle-delay* 1000) *tab-cycle-alarm*)))
    (run-ui geometry)))
