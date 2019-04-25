(ns kerbal.core
  (:require [clojure.reflect :as reflect]
            [clojure.pprint :refer [print-table]]
            [clojure.string :as str])
  (:import [krpc.client Connection]
           [krpc.client.services KRPC SpaceCenter UI]))

(defn inspect [x]
  (->> x
       reflect/reflect
       :members
       (sort-by :name)))

(defonce ^:dynamic *connection* (atom nil))

(defn log! [& args]
  (apply println args)
  (.message (UI/newInstance @*connection*)
            (str/join " " args)
            15
            krpc.client.services.UI$MessagePosition/TOP_LEFT
            (org.javatuples.Triplet. 1.0 1.0 1.0)
            (float 10.0)))

(defn connect!
  ([host rpc-port stream-port]
   (reset! *connection*
           (Connection/newInstance "Remote" host rpc-port stream-port))))

(defn disconnect! []
  (.close @*connection*)
  (reset! *connection* nil))

(defn krpc-version []
  (let [krpc (KRPC/newInstance @*connection*)]
    (-> krpc .getStatus .getVersion)))

(defn get-space-center []
  (SpaceCenter/newInstance @*connection*))

(defn get-vessel []
  (let [space-center (get-space-center)]
    (.getActiveVessel space-center)))

(defn get-control [vessel]
  (.getControl vessel))

(defn get-auto-pilot [vessel]
  (.getAutoPilot vessel))

(defn get-flight [vessel frame]
  (.flight vessel frame))

(defn next-stage! [vessel]
  (.activateNextStage (get-control vessel)))

(defn add-stream! [x n & args]
  (.addStream @*connection* x n (to-array args)))

(defn get-stage-resources [vessel stage]
  (.resourcesInDecoupleStage vessel stage false))


(defn stage-has-run-out-of-resource? [vessel stage resource-name]
  (let [resources (get-stage-resources vessel stage)]
    (and (> (.max resources resource-name) 0)
         (<= (.amount resources resource-name) 16.0))))

(defn stage-does-not-have-resource? [vessel stage resource-name]
  (let [resources (get-stage-resources vessel stage)]
    (<= (.amount resources resource-name) 0)))

(defn get-current-stage [vessel]
  (dec (.getCurrentStage (get-control vessel))))

(defn get-active-engines [vessel]
  (let [engines (.getEngines (.getParts (get-vessel)))
        active-engines (filter (fn [engine] (.getActive engine)) engines)]
    (doall active-engines)))

(defn engine-has-fuel? [engine]
  (.getHasFuel engine))

(defn solid-rocket-engine? [engine]
  (.getThrottleLocked engine))

(defn check-staging! [vessel]
  (let [stage (dec (.getCurrentStage (get-control vessel)))
        active-engines (get-active-engines vessel)
        solid-rocket-engines (filter solid-rocket-engine? active-engines)
        liquid-engines (remove solid-rocket-engine? active-engines)]
    (when (empty? active-engines)
      (log! :no-engines-staget)
      (next-stage! vessel))
    (when (and (seq solid-rocket-engines)
               (not-every? engine-has-fuel? solid-rocket-engines))
      (log! :solid-rocket-out-of-fuel)
      (next-stage! vessel))
    (when (and (seq liquid-engines)
               (not-any? engine-has-fuel? liquid-engines))
      (log! :liquid-engine-out-of-fuel)
      (next-stage! vessel))))

(defmacro while-waiting [condition & body]
  `(while ~condition
     ~@body
     (Thread/sleep 100)))

(defn launch-sequence! [vessel]
  (let [frame (.getSurfaceReferenceFrame vessel)
        flight (get-flight vessel frame)
        altitude (add-stream! flight "getMeanAltitude")
        auto-pilot (get-auto-pilot vessel)
        control (get-control vessel)]

    (log! "T-5")
    (Thread/sleep 1000)

    (log! "T-4")
    (Thread/sleep 1000)

    (log! "T-3")
    (Thread/sleep 1000)

    (log! "T-2")
    (doto control
      (.setSAS false)
      (.setRCS false)
      (.setThrottle 100))
    (Thread/sleep 1000)

    (log! "T-1")
    (doto auto-pilot
      (.engage)
      (.targetPitchAndHeading 90 90))
    (Thread/sleep 1000)

    (log! "T-0")
    (next-stage! vessel)
    (log! :engine-start)

    (while-waiting (<= (.get altitude) 10))
    (log! :liftoff)))

(defn fly-to-orbit! [vessel orbit-height]
  (let [frame (.getSurfaceReferenceFrame vessel)
        flight (get-flight vessel frame)
        ut (add-stream! SpaceCenter "getUT")
        altitude (add-stream! flight "getMeanAltitude")
        apoapsis (add-stream! (.getOrbit vessel) "getApoapsisAltitude")
        periapsis (add-stream! (.getOrbit vessel) "getPeriapsisAltitude")
        auto-pilot (get-auto-pilot vessel)
        control (get-control vessel)]

    (while-waiting (<= (.get altitude) 1000)
      (check-staging! vessel))
    (doto auto-pilot
      (.setTargetRoll 180)
      (.targetPitchAndHeading 85 90))
    (log! :gravity-turn)

    (while-waiting (<= (.get altitude) 5000)
      (check-staging! vessel))
    (doto auto-pilot
      (.targetPitchAndHeading 75 90))
    (log! :gravity-turn)

    (while-waiting (<= (.get altitude) 10000)
      (check-staging! vessel))
    (doto auto-pilot
      (.targetPitchAndHeading 45 90))
    (log! :gravity-turn)

    (while-waiting (<= (.get apoapsis) orbit-height)
      (check-staging! vessel))
    (.setThrottle control 0)
    (log! :apoapsis orbit-height :coasting)

    (while-waiting (<= (.get altitude) 70000)
      (check-staging! vessel))
    (doto auto-pilot
      (.setTargetRoll 180)
      (.targetPitchAndHeading 0 90))
    (log! :altitude (.get altitude) :prepare-to :circularize)


    (while-waiting (<= (.get altitude) (* 0.9 (.get apoapsis)))
      (check-staging! vessel))
    (.setThrottle control 100)
    (log! :altitude (.get altitude) :circularize)

    (while-waiting (<= (.get periapsis) (* 0.90 orbit-height))
      (check-staging! vessel))
    (.setThrottle control 0)
    (log! :orbit)))

(defonce flight (atom nil))

(defn go! []
  (reset! flight
          (future
            (connect! "192.168.88.146" 50000 50001)
            (prn :version (krpc-version))
            (doto (get-vessel)
              (launch-sequence!)
              (fly-to-orbit! 100000))
            (disconnect!)))
  @flight)
