;; # Runtime Configuration of log4j
;; Simplification of
;; (clj_logging_config)[https://github.com/malcolmsparks/clj-logging-config/].
;; Ran into too many sharp edges, so I simplified and baked it into here.

(ns substratum.log4j.config
  (:use [substratum.core])
  (:require [clojure.tools.logging :as log]
            [clojure.java.io :as io])
  (:import [org.apache.log4j
            Layout Level Logger WriterAppender SimpleLayout
            EnhancedPatternLayout LogManager Appender
            AppenderSkeleton]
           [org.apache.log4j.spi
            Filter LoggingEvent]
           [java.net URI URL]
           [java.io File OutputStream Writer]))

(def levels
  "Log level mapping for Log4j"
  {:all   Level/ALL
   :debug Level/DEBUG
   :error Level/ERROR
   :fatal Level/FATAL
   :info  Level/INFO
   :off   Level/OFF
   :trace Level/TRACE
   :warn  Level/WARN})

(defconst- config-logger-name
  (name (ns-name 'substratum.log4j.config)))

(defn- ^Logger config-logger []
  (Logger/getLogger config-logger-name))

(defn- ^WriterAppender make-appender
  ([]
     (make-appender (SimpleLayout.) *out*))
  ([layout]
     (make-appender layout *out*))
  ([layout writer]
     (proxy [WriterAppender] [^Layout layout ^Writer writer]
       (close [] nil))))

(defn no-appenders?
  "Returns `true` when logger has no appenders; otherwise `false`."
  [^Logger logger]
  (nil? (enumeration-seq (.getAllAppenders logger))))

(defn- init-logging! []
  (let [logger (config-logger)
        appender (make-appender)]
    (doto appender
      (.setName "_default")
      (.setImmediateFlush true))
    (doto logger
      (.removeAllAppenders)
      (.setAdditivity false)
      (.addAppender appender)
      (.setLevel Level/INFO))))

(defn- ensure-logging! []
  (when (no-appenders? (config-logger))
    (init-logging!)))

(defn LoggingEvent->map
  [^LoggingEvent event]
  (assoc (bean event) :event event))

(defn fn->Filter
  "Creates a new `Filter` object whose `decide` method is the supplied function."
  [f]
  (let [filter-fn (fn [e] (if (f e) Filter/NEUTRAL Filter/DENY))]
    (proxy [Filter] []
      (decide [^LoggingEvent e] (filter-fn e)))))

(defn fn->Layout
  "Creates a new `Layout` object whose `format` method is the supplied function."
  [f]
  (let [layout-fn (comp f LoggingEvent->map)]
    (proxy [Layout] []
      (format [e] (layout-fn e)))))

(defn fn->Appender
  "Create a new `Appender` object whose `append` function is the supplied function."
  [f]
  (let [append-fn (comp f LoggingEvent->map)]
    (proxy [AppenderSkeleton] []
      (append [^LoggingEvent e] (append-fn e))
      (close [] nil))))

(defn ensure-appender!
  ([^Logger logger]
     (ensure-appender! logger logger))
  ([^Logger logger ^Logger leaf-logger]
      (when (no-appenders? logger)
        (let [parent (.getParent logger)]
          (if (and parent (.getAdditivity logger))
            (ensure-appender! parent leaf-logger)
            (do
              (ensure-logging!)
              (log/logf
               :debug
               "Must create appender at %s, otherwise no logging would be emitted for %s"
               (.getName logger) (.getName leaf-logger))
              (.addAppender logger (make-appender))))))))

(defprotocol Coercions
  "Coerce between various logging objects."
  (^Logger as-logger [x] "Coerce argument to a logger.")
  (^Layout as-layout [x] "Coerce argument to a layout.")
  (^Level as-level [x] "Coerce argument to a level.")
  (^Appender as-appender [x ops] "Coerce argument to an appender"))

(extend-protocol Coercions
  nil
  (as-logger [_] nil)
  (as-layout [_] (SimpleLayout.))
  (as-level [_] Level/OFF)
  String
  (as-logger [^String s] (Logger/getLogger s))
  (as-layout [^String s] (EnhancedPatternLayout. s))
  (as-level [^String s] (Level/toLevel s))
  (as-appender [^String s opts] (as-appender (io/as-file s) opts))
  Logger
  (as-logger [l] l)
  Layout
  (as-layout [l] l)
  Level
  (as-level [l] l)
  clojure.lang.Namespace
  (as-logger [n] (as-logger (str n)))
  clojure.lang.Keyword
  (as-logger [k] (cond
                  (= k :root)   (Logger/getRootLogger)
                  (= k :config) (config-logger)
                  :else (throw (IllegalArgumentException.
                                "Unrecognize logger keyword."))))
  (as-level [k] (or (get levels k)
                    (throw (IllegalArgumentException.
                            "Unrecognized level keyword."))))
  (as-appender [k opts] (cond
                         (= k :console) (make-appender (:layout opts) *out*)
                         :else (throw (IllegalArgumentException.
                                       "Unrecognized appender keyword."))))
  Appender
  (as-appender [a _] a)
  URI
  (as-appender [^URI u opts] (as-appender (.toURL u) opts))
  URL
  (as-appender [^URL u opts] (if (= "file" (.getProtocol u))
                               (as-appender (io/as-file u) opts)
                               (throw
                                (IllegalArgumentException.
                                 (str "Can not write to non-file URL <" u ">")))))
  File
  (as-appender [^File x opts] (doto (WriterAppender.
                                     ^Layout (:layout opts)
                                     ^Writer (io/make-writer x opts))
                                (.setEncoding (:encoding opts))))
  OutputStream
  (as-appender [x opts] (as-appender (io/make-writer x opts) opts))
  Writer
  (as-appender [x opts] (doto (WriterAppender. ^Layout (:layout opts) x)
                          (.setEncoding (:encoding opts))))
  clojure.lang.Fn
  (as-appender [x opts] (fn->Appender x))
  )

(defn -set-logger!
  "Sets the specified logger with the given configuration."
  [logger {:keys [name level out encoding pattern layout filter additivity test append]
           :or {name "_default" level :info encoding "UTF-8" test true}
           :as opts}]
  (ensure-logging!)
  (log/logf :debug "Setting logger '%s' with args %s" logger opts)

  (when (and layout pattern)
    (throw (IllegalArgumentException.
            ":pattern and :layout keywords cannot both be specified.")))

  (let [^Layout layout (as-layout (or layout pattern))
        opts (assoc opts :layout layout)
        out (or out (and layout :console))
        appender (as-appender out opts)
        logger (as-logger logger)
        level (as-level level)
        additivity (if (nil? additivity) (and appender false) additivity)]
    (when (and appender (nil? (.getName appender)))
      (log/logf :debug "Setting name of appender to: %s" name)
      (.setName appender name))

    (when appender
      ;;(log/logp :debug "Appenders:" (map #(.getName %) (enumeration-seq (.getAllAppenders logger))))
      (.removeAppender logger ^String name)
      (log/logf :debug "Adding appender named %s to logger %s" name (.getName logger))
      (.addAppender logger appender))
    
    (log/logf :debug "Setting level to  %s" level)
    (.setLevel logger level)

    (when-not (nil? additivity)
      (log/logf :debug "Setting additivity to %s" additivity)
      (.setAdditivity logger additivity))

    (ensure-appender! logger)

    (when filter
      (log/logf :debug "Adding filter to appender")
      (.addFilter ^Appender appender (fn->Filter filter)))
    
    (when (true? test)
      (.log logger Level/ALL (str "substratum.log4j.config: Testing logger"
                                  (.getName logger) "... 1..2..3..")))))

(defmacro set-logger!
  "Sets the configuration for the specified logger, if none is specified, the
logger for the namespace is used."
  [& opts]
  `(let [[logger# & opts#] (if (odd? (count '~opts))
                             '~opts
                             (cons (name (ns-name *ns*)) '~opts))
         opts# (apply hash-map opts#)]
     (-set-logger! logger# opts#)))

(defmacro get-logger
  ([]
     `(get-logger (name (ns-name *ns*))))
  ([logger]
     `(as-logger ~logger)))

(defn reset-logging! []
  (LogManager/resetConfiguration))

(defn -set-level!
  [logger level]
  (ensure-logging!)
  (let [logger (as-logger logger)
        level  (as-level level)]
    (log/logf :debug "Set level for %s to %s." logger level)
    (.setLevel logger level)))

(defmacro set-level!
  "Set the logging level for the specified logger. If none is specified the
default logger for the namespace is used."
  ([level]
     `(set-level! (name (ns-name *ns*)) ~level))
  ([logger level]
     `(-set-level! ~logger ~level)))

(defn -set-additivity!
  [logger additive?]
  (ensure-logging!)
  (let [logger (as-logger logger)]
    (log/logf :debug "Set additivity for %s to %s" logger additive?)
    (.setAdditivity logger additive?)))

(defmacro set-additivity!
  "Set the additivity for the specified logger. If none is specified the
default logger for the namespace is used."
  ([additive?]
     `(set-additivity! (name (ns-name *ns*)) ~additive?))
  ([logger additive?]
     `(-set-additivity! ~logger ~additive?)))

;;TODO Thread-Local Loggers
