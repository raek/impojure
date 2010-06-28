(ns impojure.class.access-flag
  (:require [clojure.set :as set])
  (:use (impojure.class io constants)))

(declare read-class-access-flags read-field-access-flags read-access-flags)

(defn read-class-access-flags [in]
  (let [flags (read-access-flags class-access-flags in)]
    (assert (if (:interface flags)
	      (and (:abstract flags)
		   (not (:super flags))
		   (not (:final flags)))
	      true))
    flags))

(defn read-field-access-flags [in]
  (let [flags (read-access-flags field-access-flags in)]
    (assert (->> flags (set/intersection #{:public :private :protected})
                 count (>= 1)))
    (assert (not (and (:final flags)
                      (:volatile flags))))
    flags))

(defn read-method-access-flags [in]
  (let [flags (read-access-flags method-access-flags in)]
    (assert (->> flags (set/intersection #{:public :private :protected})
                 count (>= 1)))
    flags))

(defn read-access-flags [access-flags in]
  (let [bit-flags (read-u2 in)]
    (set (reduce concat
                 (for [[bit-mask flag] access-flags]
                   (when (not= (bit-and bit-flags bit-mask) 0)
                     [flag]))))))
