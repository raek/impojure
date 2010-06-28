(ns impojure.class.read
  (:use (impojure.class io
                        constants
                        constant-pool
                        access-flag
                        attribute)))

(declare read-class read-interfaces read-fields read-methods)

(defn read-class [in]
  (let [magic (read-u4 in)]
    (assert (= magic 0xCAFEBABE))
    (let [minor-version (read-u2 in)
	  major-version (read-u2 in)
	  constant-pool (flatten-constant-pool (read-constant-pool in))
	  access-flags  (read-class-access-flags in)
	  this-class    (lookup-class constant-pool (read-u2 in))
	  super-class   (lookup-class constant-pool (read-u2 in))
	  interfaces    (read-interfaces constant-pool in)
          fields        (read-fields constant-pool in)
          methods       (read-methods constant-pool in)
          attributes    (read-attributes :impojure/class constant-pool in)]
      {:version       [major-version minor-version]
       :flags         access-flags
       :name          this-class
       :super-class   super-class
       :interfaces    interfaces
       :fields        fields
       :methods       methods
       :attributes    attributes
       :constant-pool (rest constant-pool)})))

(defn read-interfaces [constant-pool in]
  (let [interface-count (read-u2 in)]
    (doall (for [_ (range interface-count)]
             (lookup-class constant-pool (read-u2 in))))))

(defn read-fields [constant-pool in]
  (let [field-count (read-u2 in)]
    (doall (for [_ (range field-count)]
             (let [access-flags (read-field-access-flags in)
                   field-name (lookup-name constant-pool (read-u2 in))
                   field-descriptor (lookup-descriptor constant-pool (read-u2 in))
                   field-attributes (read-attributes :impojure/field constant-pool in)]
               {:flags access-flags
                :name field-name
                :type field-descriptor
                :attributes field-attributes})))))

(defn read-methods [constant-pool in]
  (let [method-count (read-u2 in)]
    (doall (for [_ (range method-count)]
             (let [access-flags (read-method-access-flags in)
                   method-name (lookup-name constant-pool (read-u2 in))
                   method-descriptor (lookup-descriptor constant-pool (read-u2 in))
                   method-attributes (read-attributes :impojure/method constant-pool in)]
               {:flags access-flags
                :name method-name
                :type method-descriptor
                :attributes method-attributes})))))
